/*
 * Copyright, 1999- 2012, SALESFORCE.com 
 * All Rights Reserved
 * Company Confidential
 */
package svl;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.Iterator;

/**
 * TestClient
 *
 * @author vspivak
 * @since 182
 */
public class TestClient {
    public static void main(String[] args) throws IOException {
        new TestClient().test();
    }

    private void test() throws IOException {
        Selector selector = Selector.open();

        SocketChannel sChannel1 = createSocketChannel("localhost", 8888);
        SocketChannel sChannel2 = createSocketChannel("localhost", 8888);

        sChannel1.register(selector, sChannel1.validOps());
        sChannel2.register(selector, sChannel2.validOps());

        while (true) {
            try {
                if (selector.select(5000) == 0) {
                    continue;
                }
            } catch (IOException e) {
                System.out.println("e = " + e);
                break;
            }

            // Get list of selection keys with pending events
            Iterator<SelectionKey> it = selector.selectedKeys().iterator();
            while (it.hasNext()) {
                SelectionKey selKey = it.next();
                it.remove();

                try {
                    processSelectionKey(selKey);
                } catch (IOException e) {
                    // Handle error with channel and unregister
                    selKey.cancel();
                }
            }
        }
    }

    private SocketChannel createSocketChannel(String hostName, int port) throws IOException {
        SocketChannel sChannel = SocketChannel.open();
        sChannel.configureBlocking(false);

        sChannel.connect(new InetSocketAddress(hostName, port));
        return sChannel;
    }

    public void processSelectionKey(SelectionKey selKey) throws IOException {
        if (selKey.isValid() && selKey.isConnectable()) {
            SocketChannel sChannel = (SocketChannel) selKey.channel();

            boolean success = sChannel.finishConnect();
            if (!success) {
                // An error occurred; handle it
                // Unregister the channel with this selector
                selKey.cancel();
            }
        }
        if (selKey.isValid() && selKey.isReadable()) {
            SocketChannel sChannel = (SocketChannel) selKey.channel();

            read(sChannel);
        }
        if (selKey.isValid() && selKey.isWritable()) {
            SocketChannel sChannel = (SocketChannel) selKey.channel();

            write(sChannel, "5000\n");
            selKey.channel().register(selKey.selector(), SelectionKey.OP_READ | SelectionKey.OP_CONNECT);
        }
    }

    private void write(SocketChannel sChannel, String line) throws IOException {
        System.out.println("Write");
        ByteBuffer buf = BufferHelper.str_to_bb(line);
        sChannel.write(buf);
    }

    private void read(SocketChannel sChannel) throws IOException {
        System.out.println("Read");
        ByteBuffer buf = ByteBuffer.allocateDirect(1024);

        buf.clear();
        int numBytesRead = sChannel.read(buf);

        if (numBytesRead == -1) {
            // No more bytes can be read from the channel
            sChannel.close();
        } else {
            buf.flip();

            String str = BufferHelper.bb_to_str(buf);
            System.out.println(str);
        }
    }
}
