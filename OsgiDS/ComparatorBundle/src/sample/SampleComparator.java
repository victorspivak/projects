/*
 * Copyright  1994-2009. Victor Spivak.  All Rights Reserved.
 */

package sample;

import java.util.Comparator;

public class SampleComparator implements Comparator {
    public int compare(Object o1, Object o2) {
        return o1.equals(o2) ? 0 : -1;
    }
}
