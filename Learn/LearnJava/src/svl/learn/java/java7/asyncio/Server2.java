package svl.learn.java.java7.asyncio;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.charset.Charset;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class Server2
{
    final static public int SERVER_PORT = 9001;
    final static public String SERVER_IP = "127.0.0.1";
    private static final int TIMEOUT = 5000;
    private static final int CAPACITY = 1024;

    public static void main(String[] args)
    {
        //create asynchronous server-socket channel bound to the default group
        try (AsynchronousServerSocketChannel asynchronousServerSocketChannel = AsynchronousServerSocketChannel.open())
        {
            if (asynchronousServerSocketChannel.isOpen())
            {
                //bind to local address
                asynchronousServerSocketChannel.bind(new InetSocketAddress(SERVER_IP, SERVER_PORT));

                //display a waiting message
                System.out.println("Waiting for connections ...");
                while (true)
                {
                    Future<AsynchronousSocketChannel> asynchronousSocketChannelFuture = asynchronousServerSocketChannel.accept();
                    try (AsynchronousSocketChannel asynchronousSocketChannel = asynchronousSocketChannelFuture.get())
                    {
                        System.out.printf("Incoming connection from: %s\n", asynchronousSocketChannel.getRemoteAddress());
                        ByteBuffer incomingBuffer = ByteBuffer.allocateDirect(CAPACITY);
                        //receiving data
                        asynchronousSocketChannel.read(incomingBuffer, incomingBuffer,  new CompletionHandler<Integer, ByteBuffer>()
                        {
                            public void completed(Integer result, ByteBuffer buffer)
                            {
                                buffer.flip();
                                String msgReceived = Charset.defaultCharset().decode(buffer).toString();
                                System.out.println("Msg received from the client : " + msgReceived);
                            }
                            public void failed(Throwable exc, ByteBuffer buffer)
                            {
                                throw new UnsupportedOperationException("read failed!");
                            }
                        });
                        try
                        {
                            Thread.sleep(TIMEOUT);
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }

                        //replying data
                        ByteBuffer outgoingBuffer = ByteBuffer.wrap("World".getBytes());
                        asynchronousSocketChannel.write(outgoingBuffer).get();
                    }
                    catch (IOException | InterruptedException | ExecutionException ex)
                    {
                        System.err.println(ex);
                    }
                }
            }
            else
            {
                System.out.println("The asynchronous server-socket channel cannot be opened!");
            }
        }
        catch (IOException ex)
        {
            System.err.println(ex);
        }
    }
}
