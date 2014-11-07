/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.problems;

public class RemoveDupChars {
    public static void main(String[] args) {
        test("aaab", 2);
        test("aabb", 1);
        test("aabbaa", 1);
    }

    public static void test(String in, int allowedCount) {
        System.out.println(in + " (" + allowedCount + ")" + " --> " + removeDups(in, allowedCount));
    }

    public static String removeDups(String in, int allowedCount) {
        if (in == null || allowedCount < 1)
            throw new IllegalArgumentException();

        if (in.length() < allowedCount + 1)
            return in;

        StringBuilder buffer = new StringBuilder(in.length());
        char seenChar = in.charAt(0);
        int seenCharCount = 1;
        buffer.append(seenChar);

        for (int i = 1; i < in.length(); i++) {
            char current = in.charAt(i);

            if (seenChar == current) {
                seenCharCount++;
                if (seenCharCount > allowedCount)
                    continue;
            } else {
                seenChar = current;
                seenCharCount = 1;
            }

            buffer.append(current);
        }

        return buffer.toString();
    }
}