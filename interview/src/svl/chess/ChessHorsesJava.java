package svl.chess;

import java.util.*;

public class ChessHorsesJava
{
	public static void main (String[] args)
	{
		ChessHorsesJava horses  = new ChessHorsesJava();

	    long time1 = System.currentTimeMillis();
		Stack<Position> path = horses.find(new Position(1, 1));
		System.out.println ("Time = " + (System.currentTimeMillis() - time1));

		System.out.println(path);

	}

	public Stack<Position> find (Position start)
	{
	    System.out.println ("Start: " + start.toPrettyString());

	    Set<Position> board = new HashSet<Position>();
	    Stack<Position> path = new Stack<Position>();
		Set<Board>    analyzed    = new HashSet<Board>();

		analyze(start, board, path, analyzed);

	    return path;
	}

	public boolean analyze (Position pos, Set<Position> state, Stack<Position> path, Set<Board> analyzed)
	{
//		System.out.println ("" + pos.toPrettyString() + "  " + path.size());

		state.add(pos);
		path.push(pos);

		if (path.size() >= DUP_LEVEL_CHECK1 && path.size() <= DUP_LEVEL_CHECK2)
		{
			Board board = new Board(path, pos);

			if (analyzed.size() >= MAX_CACHE_SIZE)
			{
				if (analyzed.contains(board))
					return processDup(pos, state, path);
			}
			else if (!analyzed.add(board))
				return processDup(pos, state, path);
		}

		if (path.size() == MOVE_COUNT)
		{
			System.out.println("********************************************************************************");
			System.out.println("Found");
			return true;
		}

		iterCount++;
//		if (iterCount == 1000000)
//			return true;
		
		if ((iterCount % 1000000) == 0)
		{
			System.out.println("Iteration " + iterCount / 1000000 + "M" + "  " + path.size() + "  " + analyzed.size() +
			                   "  " + dupCount + "  " + minDup);
		}

		if (process2(pos, state, path, analyzed))
			return true;

//		returnCount++;
//		if ((returnCount % 40) == 0)
//			System.out.println();
//		System.out.print("  " + path.size());

		restoreState(pos, state, path);

	    return false;
	}

	private boolean process1 (Position pos, Set<Position> state, Stack<Position> path, Set<Board> analyzed)
	{
		List<Position> moves = pos.getPossibleMoves();

		for (Position move : moves)
		{
			if (!state.contains(move))
			{
				if (analyze(move, state, path, analyzed))
					return true;
			}
		}
		return false;
	}

	private boolean process2 (Position pos, Set<Position> state, Stack<Position> path, Set<Board> analyzed)
	{
		byte x = pos.getX();
		byte y = pos.getY();

		if (x > 1)
		{
			if (y <= BOARD_SIZE - 2)
				if (analyzeHelper(new Position(x - 1, y + 2), state, path, analyzed))
					return true;
			if (y > 2)
				if (analyzeHelper(new Position(x - 1, y - 2), state, path, analyzed))
					return true;
		}
		if (x > 2)
		{
			if (y > 1)
				if (analyzeHelper(new Position(x - 2, y - 1), state, path, analyzed))
					return true;
			if (y <= BOARD_SIZE - 1)
				if (analyzeHelper(new Position(x - 2, y + 1), state, path, analyzed))
					return true;
		}
		if (x <= BOARD_SIZE - 1)
		{
			if (y > 2)
				if (analyzeHelper(new Position(x + 1, y - 2), state, path, analyzed))
					return true;
			if (y <= BOARD_SIZE - 2)
				if (analyzeHelper(new Position(x + 1, y + 2), state, path, analyzed))
					return true;
		}
		if (x <= BOARD_SIZE - 2)
		{
			if (y > 1)
				if (analyzeHelper(new Position(x + 2, y - 1), state, path, analyzed))
					return true;
			if (y <= BOARD_SIZE - 1)
				if (analyzeHelper(new Position(x + 2, y + 1), state, path, analyzed))
					return true;
		}
		return false;
	}

	public boolean analyzeHelper (Position pos, Set<Position> state, Stack<Position> path, Set<Board> analyzed)
	{
		return !state.contains(pos) && analyze(pos, state, path, analyzed);
	}

	private boolean processDup (Position pos, Set<Position> state, Stack<Position> path)
	{
		if (minDup == path.size())
				System.out.println("Dup *****> " + path.size());
		if (minDup > path.size())
			minDup = path.size();
		dupCount++;
//				System.out.println("Dup *****> " + path.size() + "   " + analyzed.size());
		restoreState(pos, state, path);
		return false;
	}

	private void restoreState (Position pos, Set<Position> board, Stack<Position> path)
	{
		board.remove(pos);
		path.pop();
	}

	final static public int DUP_LEVEL_CHECK1 = 30;
	final static public int DUP_LEVEL_CHECK2 = 39;
	final static public int BOARD_SIZE = 8;
	final static public int MOVE_COUNT = BOARD_SIZE * BOARD_SIZE;
	private long iterCount = 0;
	private int dupCount = 0;
	private int minDup = Integer.MAX_VALUE;
	private int returnCount = 0;
	private static final int MAX_CACHE_SIZE = 2000000;
}