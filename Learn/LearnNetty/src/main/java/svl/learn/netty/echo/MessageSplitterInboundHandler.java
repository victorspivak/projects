package svl.learn.netty.echo;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.ByteToMessageDecoder;
import io.netty.util.CharsetUtil;

import java.util.List;

public class MessageSplitterInboundHandler extends ByteToMessageDecoder {
    @Override
    protected void decode(ChannelHandlerContext ctx, ByteBuf in, List<Object> out) throws Exception {
        String str = in.toString(CharsetUtil.UTF_8);
        in.readerIndex(in.writerIndex());
        for (int i = 0; i <str.length(); i++) {
            out.add(Unpooled.copiedBuffer(String.valueOf(str.charAt(i)), CharsetUtil.UTF_8));
        }
    }
}