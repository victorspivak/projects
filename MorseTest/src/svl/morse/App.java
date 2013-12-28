package svl.morse;

import org.apache.log4j.*;

import java.io.*;
import java.text.*;
import java.util.*;

public class App
{
	public static void main (String[] args)
	{
		InputStream inputStream = null;
		String filename = "input/progc.dat"; //todo input

		try
		{
			InputProcessor  processor = new InputProcessor();
			inputStream = new FileInputStream(filename);
			processor.load(inputStream);

			MorseDecoder    decoder = new MorseDecoder(processor.getMorseTable(), processor.getContextWords());
			List<String>    input   = processor.getInputWords();
			for (String code : input)
			{
				String word = decoder.decode(code);
				System.out.println(word);
			}
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
		finally
		{
			if (inputStream != null)
				try
				{
					inputStream.close();
				}
				catch (IOException e)
				{
					Logger.getLogger(App.class).error("Error occurred: ", e);   //todo externilize string
				}
		}
	}
}