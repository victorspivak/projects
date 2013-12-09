package svl.chess;

import java.util.*;

public class Position
{
	public Position (int x, int y)
	{
		this.x = (byte) x;
		this.y = (byte) y;
	}

//	public Position (byte index)
//	{
//		this.x = (byte) (index % ChessHorsesJava.BOARD_SIZE);
//		this.y = (byte) (index / ChessHorsesJava.BOARD_SIZE + 1);
//	}
//
	public byte getX ()
	{
		return x;
	}

	public byte getY ()
	{
		return y;
	}

	public int getIndex ()
	{
		return ((y - 1) * ChessHorsesJava.BOARD_SIZE + x);
	}
	
//	public List<Position> getPossibleMoves ()
//	{
//	    List<Position> moves = new ArrayList<Position>(8);
//
//		addValidMove(moves, new Position(x - 1, y - 1));
//		addValidMove(moves, new Position(x - 1, y));
//		addValidMove(moves, new Position(x - 1, y + 1));
//		addValidMove(moves, new Position(x, y + 1));
//		addValidMove(moves, new Position(x, y - 1));
//		addValidMove(moves, new Position(x + 1, y - 1));
//		addValidMove(moves, new Position(x + 1, y));
//		addValidMove(moves, new Position(x + 1, y + 1));
//
//		return moves;
//	}

	public List<Position> getPossibleMoves ()
	{
	    List<Position> moves = new ArrayList<Position>(8);

		addValidMove(moves, new Position(x - 1, y - 2));
		addValidMove(moves, new Position(x - 1, y + 2));
		addValidMove(moves, new Position(x - 2, y - 1));
		addValidMove(moves, new Position(x - 2, y + 1));
		addValidMove(moves, new Position(x + 1, y - 2));
		addValidMove(moves, new Position(x + 1, y + 2));
		addValidMove(moves, new Position(x + 2, y - 1));
		addValidMove(moves, new Position(x + 2, y + 1));

		return moves;
	}

	private void addValidMove (List<Position> moves, Position pos)
	{
		if (pos.getX() > 0 && pos.getX() <= ChessHorsesJava.BOARD_SIZE &&
				pos.getY() > 0 && pos.getY() <= ChessHorsesJava.BOARD_SIZE)
		{
			moves.add(pos);
		}
	}

	public String toPrettyString ()
	{
	    return "" + Character.toString((char) ('a' + x - 1)) + y;
	}

	@Override
	public String toString ()
	{
		return toPrettyString();
	}

	@SuppressWarnings({"EqualsWhichDoesntCheckParameterClass"})
	@Override
	public boolean equals (Object o)
	{
		Position position = (Position) o;

		if (x != position.x)
			return false;
		if (y != position.y)
			return false;

		return true;
	}

	@Override
	public int hashCode ()
	{
		int result = (int) x;
		result = 31 * result + (int) y;

		return result;
	}

	private byte x;
	private byte y;
}