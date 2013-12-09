package svl.chess;

import java.util.*;

public class Board
{
	public Board (Stack<Position> path, Position pos)
	{
		value = 0;
		posIndex = pos.getIndex();

		for (Position position : path)
		{
			int index = position.getIndex();
			long mask = 1L << index;
			value |= mask;
		}
	}

	public boolean isSet (Position position)
	{
		return (value & (1L << position.getIndex())) > 0;	
	}

	@SuppressWarnings({"EqualsWhichDoesntCheckParameterClass"})
	@Override
	public boolean equals (Object o)
	{
		Board board = (Board) o;

		if (posIndex != board.posIndex)
			return false;
		if (value != board.value)
			return false;

		return true;
	}

	@Override
	public int hashCode ()
	{
		int result = (int) (value ^ (value >>> 32));
		result = 31 * result + (int) (posIndex ^ (posIndex >>> 32));
		return result;
	}

	private long value;
	private long posIndex;
}