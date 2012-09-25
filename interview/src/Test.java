/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */



import svl.ExceptionHelper;

import java.sql.SQLException;

public class Test {
    public static void main(String[] args) {
        Test test = new Test();

        test.aMethod();
    }

    public void aMethod() {
        ExceptionHelper.doThrow(new SQLException());
    }
}
