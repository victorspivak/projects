package svl.morse;

import org.apache.log4j.*;
import org.junit.*;

import java.io.*;
import java.text.*;

import static org.junit.Assert.assertEquals;

public class TestDecoder
{
	@Before public void load ()
	{
		InputStream inputStream = null;
		String filename = "input/progc.dat"; //todo input

		try
		{
			InputProcessor  processor = new InputProcessor();
			inputStream = new FileInputStream(filename);
			processor.load(inputStream);

			m_decoder = new MorseDecoder(processor.getMorseTable(), processor.getContextWords());
		}
		catch (FileNotFoundException e)
		{
			Logger.getLogger(App.class).error(MessageFormat.format("Could not open \"{0}\" file.", filename), e);   //todo externilize string
		}
		catch (IOException e)
		{
			Logger.getLogger(App.class).error(MessageFormat.format("Could not read from \"{0}\" file.", filename), e);   //todo externilize string
		}
		catch (InvalidInputException e)
		{
			Logger.getLogger(App.class).error("Error occurred: ", e);   //todo externilize string
		}
	}

	@Test   public void fullMatchTest ()
	{
		String value = m_decoder.decode(".--.....--");

		assertEquals("WHAT", value);
	}

	@Test   public void fullMatchTest1 ()
	{
		String value = m_decoder.decode(".--.");

		assertEquals("AN", value);
	}

	@Test   public void findShortestStringTest ()
	{
		String value = m_decoder.decode("..--");

		assertEquals("IM!", value);
	}

	@Test   public void nonCompleteCodeTest ()
	{
		String value = m_decoder.decode(".--.-.----..");

		assertEquals("WROTH?", value);
	}

	private MorseDecoder m_decoder;
}