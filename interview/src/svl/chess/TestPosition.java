package svl.chess;

import org.junit.*;

import static org.junit.Assert.*;

public class TestPosition
{
	@Test public void toPrettyStringTest ()
	{
		Position2 position   = new Position2(4,3);

		assertEquals("d,3", position.toPrettyString());
	}

	@Test public void indexConstructorTest ()
	{
		testPosition(4, 1, 4);
		testPosition(4, 3, 20);
	}

	private void testPosition (int x, int y, int index)
	{
		Position2 p1 = new Position2(x, y);
		Position2 p2 = new Position2((byte) index);

		assertEquals(p1, p2);
	}
}