package svl.morse;

import org.apache.log4j.*;

import java.util.*;

public class MorseDecoder
{
	public MorseDecoder (Map<String, Character> morseTable, List<String> contextWords)
	{
		m_morseTable = morseTable;
		m_contextWords = contextWords;
		m_logger    = Logger.getLogger(MorseDecoder.class);

		buildContextWordsIndex();
	}

	public String decode (String inputCode)
	{
		MatchingState matchingState = new MatchingState();
		decode(inputCode.toCharArray(), 0, "", "", matchingState);
		String result = matchingState.getFoundWord();

		if (result != null)
		{
			if (matchingState.isAmbigious())
				result  += "!";
		}
		else
		{
			result  = matchingState.getPartialWord();

			if (result != null)
				result =  m_partialContextWordsIndex.get(result) + "?";
			else
				result = "?";
		}

		return result;
	}

	public String decode (char[]  chars, int position, String word, String code, MatchingState matchingState)
	{
		if (m_logger.isDebugEnabled())
			m_logger.debug(String.format("decode pos %d word %s code %s  state %s", position, word, code, matchingState.toString()));

		if (chars.length <= position)
		{
			if (m_contextWordsIndex.contains(word))
				matchingState.setFoundWord(word);
			return null;
		}

		code = code + chars[position];
		Character decodedCh    = m_morseTable.get(code);

		if (decodedCh != null)
		{
			String newWord    = word + decodedCh;

			if (m_partialContextWordsIndex.containsKey(newWord))
			{
				matchingState.setPartialWord(newWord, position);

				String decodedWord =  decode(chars, position + 1, newWord, "", matchingState);

				if (decodedWord != null)
					return decodedWord;
				else
					return decode(chars, position + 1, word, code, matchingState);
			}
			else
				return decode(chars, position + 1, word, code, matchingState);
		}
		else
			return null;
	}

	private void buildContextWordsIndex ()
	{
		m_partialContextWordsIndex = new HashMap<String, String>();
		m_contextWordsIndex = new HashSet<String>();

		for (String word : m_contextWords)
		{
			int length = word.length();
			for (int i = 1; i < length; i++)
				m_partialContextWordsIndex.put(word.substring(0, i), word);
			m_partialContextWordsIndex.put(word, word);
			m_contextWordsIndex.add(word);
		}
	}

	private Map<String, Character> m_morseTable;
	private List<String> m_contextWords;
	private Map<String, String> m_partialContextWordsIndex;
	private Set<String> m_contextWordsIndex;
	private Logger  m_logger;

	private class MatchingState
	{
		private MatchingState ()
		{
			m_ambigious = false;
			m_foundWord = null;
		}

		public String getFoundWord ()
		{
			return m_foundWord;
		}

		public void setFoundWord (String word)
		{
			if (m_logger.isDebugEnabled())
				m_logger.debug(String.format("setFoundWord %s", word));

			if (m_foundWord != null && !word.equals(m_foundWord))
				m_ambigious = true;

			m_foundWord = word;
		}

		public String getPartialWord ()
		{
			return m_partialWord;
		}

		public void setPartialWord (String partialWord, int partialPosition)
		{
			if (m_partialWord == null || partialPosition > m_partialPosition)
			{
				if (m_logger.isDebugEnabled())
					m_logger.debug(String.format("setPartialWord %s %d", partialWord, partialPosition));

				m_partialWord   = partialWord;
				m_partialPosition   = partialPosition;
			}
		}

		public boolean isAmbigious ()
		{
			return m_ambigious;
		}

		@Override
		public String toString ()
		{
			return String.format("Found %s (%b) Partial %s (%d)", m_foundWord, m_ambigious, m_partialWord, m_partialPosition);
		}

		private String m_foundWord;
		private boolean m_ambigious;
		private String m_partialWord;
		private int m_partialPosition;
	}
}