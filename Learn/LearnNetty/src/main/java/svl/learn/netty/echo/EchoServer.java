/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.netty.echo;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.util.ResourceLeakDetector;

import java.net.InetSocketAddress;

public class EchoServer {
    private static final int BACKLOG_SIZE = 128;
    private final int port;
    public EchoServer(int port) {
        this.port = port;
    }
    public static void main(String[] args) throws Exception {
        if (args.length != 1)
            System.err.println("Usage: " + EchoServer.class.getSimpleName() + " <port>");

        int port = Integer.parseInt(args[0]);
        new EchoServer(port).start();
    }
    public void start() throws Exception {
        System.out.println("Start Server...");
        ResourceLeakDetector.setLevel(ResourceLeakDetector.Level.PARANOID);

        final EventLoopGroup group = new NioEventLoopGroup();
        try {
            ServerBootstrap bootstrap = new ServerBootstrap();
            bootstrap.group(group)
                .channel(NioServerSocketChannel.class)
                .localAddress(new InetSocketAddress(port))
                .childHandler(new ChannelInitializer<SocketChannel>() {
                    @Override
                    public void initChannel(SocketChannel channel) throws Exception {
                        channel.pipeline().addLast(new LoggingChannelInboundHandler());
                        channel.pipeline().addLast(new MessageSplitterInboundHandler());
                        channel.pipeline().addLast(new EchoServerHandler(group));
                        channel.pipeline().addLast(new LoggingChannelInboundHandler());
                    }
                })
                .option(ChannelOption.SO_BACKLOG, BACKLOG_SIZE)
                .childOption(ChannelOption.SO_KEEPALIVE, true);

            ChannelFuture channelFuture = bootstrap.bind();
            channelFuture.channel().closeFuture().sync();
        } finally {
            group.shutdownGracefully().sync();
            System.out.println("Server finished");
        }
    }
}
