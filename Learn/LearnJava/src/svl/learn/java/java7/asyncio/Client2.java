package svl.learn.java.java7.asyncio;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.charset.Charset;
import java.util.concurrent.ExecutionException;

public class Client2 {
    private static final int CAPACITY = 1024;
    static boolean completed;

    public static void main(String[] args) {
        ByteBuffer receivingBuffer = ByteBuffer.allocateDirect(CAPACITY);
        ByteBuffer sendingBuffer = ByteBuffer.wrap("Hello".getBytes());

        //create asynchronous socket channel
        try (final AsynchronousSocketChannel asynchronousSocketChannel = AsynchronousSocketChannel.open()) {
            if (asynchronousSocketChannel.isOpen()) {
                //connect this channel's socket
                Void connect = asynchronousSocketChannel.connect(new InetSocketAddress(Server2.SERVER_IP, Server2.SERVER_PORT)).get();
                if (connect == null) {
                    System.out.println("Local address: " + asynchronousSocketChannel.getLocalAddress());

                    //sending data
                    asynchronousSocketChannel.write(sendingBuffer).get();
                    asynchronousSocketChannel.read(receivingBuffer, receivingBuffer, new CompletionHandler<Integer, ByteBuffer>() {
                        public void completed(Integer result, ByteBuffer buffer) {
                            buffer.flip();
                            String msgReceived = Charset.defaultCharset().decode(buffer).toString();
                            System.out.println("Msg received from server : " + msgReceived);
                            completed = true;
                        }

                        public void failed(Throwable exc, ByteBuffer buffer) {
                            completed = false;
                            throw new UnsupportedOperationException("read failed!");
                        }
                    });

                    while (!completed) {
                        try {
                            Thread.sleep(1000);
                        } catch (InterruptedException ignored) {
                            System.out.println("Waiting for response from the server");
                        }

                    }
                }
                else
                {
                    System.out.println("The connection cannot be established!");
                }
            } else {
                System.out.println("The asynchronous socket channel cannot be opened!");
            }
        } catch (IOException | InterruptedException | ExecutionException e) {
            e.printStackTrace();
        }
    }
}

