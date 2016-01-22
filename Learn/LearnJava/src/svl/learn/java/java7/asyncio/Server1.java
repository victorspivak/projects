package svl.learn.java.java7.asyncio;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class Server1 {

    public static final int PORT = 8380;
    private static final int CAPACITY = 32;

    public static void main(String[] args) throws Exception {
        runServer();
    }

    private static void runServer() throws IOException, InterruptedException {
        final AsynchronousServerSocketChannel listener = AsynchronousServerSocketChannel.open();
        InetSocketAddress address = new InetSocketAddress("localhost", PORT);
        listener.bind(address);
        listener.accept(null, new CompletionHandler<AsynchronousSocketChannel, Void>() {
            public void completed(AsynchronousSocketChannel channel, Void attribute) {
                listener.accept(null, this);
                handleMessage(channel);
            }

            private void handleMessage(AsynchronousSocketChannel channel) {
                try {
                    System.out.println("Server: completed method executing");
                    while (true) {
                        ByteBuffer buffer = ByteBuffer.allocate(CAPACITY);
                        Future<Integer> readFuture = channel.read(buffer);
                        Integer number = readFuture.get();
                        if (number >= 0){
                            System.out.println(Thread.currentThread().getName());
                            System.out.printf("Server: Message received: %s\n", new String(buffer.array()));
                        } else
                            break;
                    }
                } catch (InterruptedException | ExecutionException ex) {
                    ex.printStackTrace();
                }
            }

            public void failed(Throwable ex, Void attribute) {
                System.out.println("Server: CompletionHandler exception");
                ex.printStackTrace();
            }
        });

        while (true) {
            Thread.sleep(1000);
            System.out.print(".");
        }
    }
}