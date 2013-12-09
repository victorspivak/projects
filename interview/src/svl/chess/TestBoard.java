package svl.chess;

import org.junit.*;

import java.util.*;

import static org.junit.Assert.*;

public class TestBoard
{
	@Test public void indexConstructorTest ()
	{
		Stack<Position> path = new Stack<Position>();
		Position position = new Position(8, 8);
		path.add(position);

		Board board = new Board(path, position);
		assertTrue(board.isSet(position));
	}
}