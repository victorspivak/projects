package svl.morse;

import java.io.*;
import java.util.*;

public class InputProcessor
{
	public InputProcessor ()
	{
		m_morseTable    = new HashMap<String, Character>();
		m_contextWords  = new ArrayList<String>();
		m_inputWords  = new ArrayList<String>();
	}

	public void load (InputStream inputStream) throws IOException, InvalidInputException
	{
		BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));

		loadMorseTable(reader);
		loadContextWords(reader);
		loadInputWords(reader);
	}

	public Map<String, Character> getMorseTable ()
	{
		return m_morseTable;
	}

	public List<String> getContextWords ()
	{
		return m_contextWords;
	}

	public List<String> getInputWords ()
	{
		return m_inputWords;
	}

	private void loadMorseTable (BufferedReader reader) throws IOException, InvalidInputException
	{
		String line = null;

		while ((line = reader.readLine()) != null)
		{
			line = line.trim();

			if (!line.isEmpty())
			{
				if (line.equals(END_SECTION))
					break;
				processMorseTableLine(line);
			}
		}
	}

	private void processMorseTableLine (String line) throws InvalidInputException
	{
		Scanner scanner = new Scanner(line);
		Character ch = scanner.next(".").charAt(0);
		String code = scanner.next("[.-]+");

		if (Character.isUpperCase(ch) || Character.isDigit(ch))
			m_morseTable.put(code, ch);
		else
			throw new InvalidInputException("Invalid Morse Code: " + ch);
	}

	private void loadContextWords (BufferedReader reader) throws IOException
	{
		String line = null;

		while ((line = reader.readLine()) != null)
		{
			line = line.trim();

			if (!line.isEmpty())
			{
				if (line.equals(END_SECTION))
					break;
				m_contextWords.add(line);
			}
		}
	}

	private void loadInputWords (BufferedReader reader) throws IOException
	{
		String line = null;

		while ((line = reader.readLine()) != null)
		{
			line = line.trim();

			if (!line.isEmpty())
			{
				if (line.equals(END_SECTION))
					break;

				processInputWordsLine(line);
			}
		}
	}

	private void processInputWordsLine (String line)
	{
		Scanner scanner = new Scanner(line);
		
		while (scanner.hasNext("[.-]+"))
			m_inputWords.add(scanner.next());
	}

	private static final String END_SECTION = "*";
	private Map<String, Character>  m_morseTable;
	private List<String> m_contextWords;
	private List<String> m_inputWords;
}