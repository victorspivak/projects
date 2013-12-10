package svl.chess;

import java.util.*;

public class Position2
{
	public Position2 (int x, int y)
	{
		value = (byte) ((y - 1) * ChessHorsesJava.BOARD_SIZE + x);
	}

	public Position2 (byte value)
	{
		this.value = value;
	}

	public byte getX ()
	{
		return (byte) (value % ChessHorsesJava.BOARD_SIZE);
	}

	public byte getY ()
	{
		return (byte) (value / ChessHorsesJava.BOARD_SIZE + 1);
	}

	public List<Position2> getPossibleMoves ()
	{
	    List<Position2> moves = new ArrayList<Position2>(8);

		byte x = getX();
		byte y = getY();

		addValidMove(moves, new Position2(x - 1, y + 2));
		addValidMove(moves, new Position2(x - 1, y - 2));
		addValidMove(moves, new Position2(x - 2, y + 1));
		addValidMove(moves, new Position2(x - 2, y - 1));
		addValidMove(moves, new Position2(x + 1, y + 2));
		addValidMove(moves, new Position2(x + 1, y - 2));
		addValidMove(moves, new Position2(x + 2, y + 1));
		addValidMove(moves, new Position2(x + 2, y - 1));

		return moves;
	}

	private void addValidMove (List<Position2> moves, Position2 pos)
	{
		if (pos.getX() > 0 && pos.getX() <= ChessHorsesJava.BOARD_SIZE &&
				pos.getY() > 0 && pos.getY() <= ChessHorsesJava.BOARD_SIZE)
		{
			moves.add(pos);
		}
	}

	public String toPrettyString ()
	{
	    return "" + Character.toString((char) ('a' + getX() - 1)) + ',' + getY();
	}

	@Override
	public String toString ()
	{
		return toPrettyString();
	}

	@Override
	public boolean equals (Object o)
	{
		Position2 position = (Position2) o;

		return value == position.value;
	}

	@Override
	public int hashCode ()
	{
		return value;
	}

	private byte value;
}