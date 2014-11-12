/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.netty.basics;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.util.CharsetUtil;

public class Buffers {
    public static void main(String[] args) {
        //Unpooled.copiedBuffer("Netty rocks!", CharsetUtil.UTF_8);

        ByteBuf buffer = Unpooled.buffer();
        dumpPointers(buffer);

        buffer.writeLong(100);
        buffer.writeLong(200);
        buffer.writeLong(300);
        dumpPointers(buffer);

        System.out.println("l = " + buffer.getLong(0));
        System.out.println("l = " + buffer.getLong(8));
        System.out.println("l = " + buffer.getLong(16));
        dumpPointers(buffer);

        buffer.readLong();
        System.out.println("l = " + buffer.getLong(0));
        System.out.println("l = " + buffer.getLong(8));
        dumpPointers(buffer);

        buffer.discardReadBytes();
        System.out.println("l = " + buffer.getLong(0));
        System.out.println("l = " + buffer.getLong(8));
        dumpPointers(buffer);

        buffer.readLong();
        System.out.println("l = " + buffer.getLong(0));
        System.out.println("l = " + buffer.getLong(8));
        dumpPointers(buffer);

        buffer.readLong();
        System.out.println("l = " + buffer.getLong(0));
        System.out.println("l = " + buffer.getLong(8));
        dumpPointers(buffer);

        buffer.writeLong(257);
        for (int i = 0; i < 8; i++)
            System.out.println("byte: " + buffer.readByte());

        dumpPointers(buffer);
        buffer.discardReadBytes();
        dumpPointers(buffer);
    }

    private static void dumpPointers(ByteBuf buffer) {
        System.out.printf("%d %d ref: %d indecies: %d %d\n", buffer.readableBytes(), buffer.writableBytes(), buffer.refCnt(), buffer.readerIndex(), buffer.writerIndex());
    }
}