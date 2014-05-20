/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.java.basic;

import java.util.Arrays;
import java.util.List;

public class TestInterfaceWithDefaultMethod {
    private interface Executor{
        int execute(String command);
        default int execute(List<String> commands){
            int total = 0;
            for (String command : commands) {
                total += execute(command);
            }

            return total;
        }
    }

    public static void main(String[] args) {
        Executor executor = new Executor() {
            @Override
            public int execute(String command) {
                return command.length();
            }
        };

        System.out.println(executor.execute("print"));
        System.out.println(executor.execute(Arrays.asList("a", "b", "c")));
    }
}