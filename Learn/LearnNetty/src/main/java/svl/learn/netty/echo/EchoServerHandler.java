/*
 * Copyright © 1994-2009. Victor Spivak.  All Rights Reserved.
 */

package svl.learn.netty.echo;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.*;
import io.netty.util.CharsetUtil;

public class EchoServerHandler extends ChannelInboundHandlerAdapter {
    private final EventLoopGroup group;
    private StringBuilder stringBuilder = new StringBuilder();

    public EchoServerHandler(EventLoopGroup group) {
        this.group = group;
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
        ByteBuf buf = (ByteBuf) msg;
        stringBuilder.append(buf.toString(CharsetUtil.UTF_8));
        ctx.fireChannelRead(buf);
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) {
        String message = stringBuilder.toString();
        if (message.equals("Exit")) {
            ctx.close();
            group.shutdownGracefully();
        } else {
            ctx.write(Unpooled.copiedBuffer(message, CharsetUtil.UTF_8));

            ctx.writeAndFlush(Unpooled.EMPTY_BUFFER).addListener(ChannelFutureListener.CLOSE);
            ctx.fireChannelReadComplete();
        }
    }
}
