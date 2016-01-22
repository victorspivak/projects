package svl.learn.java.java7.asyncio;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.Scanner;
import java.util.concurrent.Future;

public class Client1 {
    public static void main(String[] args) throws Exception {
        try {
            AsynchronousSocketChannel client = AsynchronousSocketChannel.open();
            InetSocketAddress address = new InetSocketAddress("localhost", Server1.PORT);
            Future<Void> future = client.connect(address);
            System.out.println("Client: Waiting for the connection to complete");
            future.get();
            String message;
            do {
                System.out.print("Enter a message: ");
                Scanner scanner = new Scanner(System.in);
                message = scanner.nextLine();
                System.out.println("Client: Sending ...");
                ByteBuffer buffer = ByteBuffer.wrap(message.getBytes());
                System.out.println("Client: Message sent: " + new String(buffer.array()));
                client.write(buffer);
            } while (!"quit".equals(message));
        } catch (InterruptedException e) {
            System.out.println(e);
        }
    }
}