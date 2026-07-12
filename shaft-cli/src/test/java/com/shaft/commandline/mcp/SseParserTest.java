package com.shaft.commandline.mcp;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SseParserTest {

    @Test
    void extractsSingleDataFrame() {
        String body = "id:abc123\nevent:message\ndata:{\"jsonrpc\":\"2.0\",\"id\":2}\n\n";
        assertEquals("{\"jsonrpc\":\"2.0\",\"id\":2}", SseParser.extractData(body));
    }

    @Test
    void stripsSingleLeadingSpaceAfterColon() {
        assertEquals("{\"a\":1}", SseParser.extractData("data: {\"a\":1}\n"));
    }

    @Test
    void joinsMultipleDataLinesWithNewline() {
        assertEquals("line1\nline2", SseParser.extractData("data:line1\ndata:line2\n"));
    }

    @Test
    void handlesCarriageReturns() {
        assertEquals("{\"a\":1}", SseParser.extractData("event:message\r\ndata:{\"a\":1}\r\n"));
    }
}
