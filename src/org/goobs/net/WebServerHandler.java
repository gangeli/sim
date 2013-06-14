package org.goobs.net;

import com.sun.net.httpserver.Headers;

import java.util.HashMap;

public interface WebServerHandler {
	public String handle(HashMap<String,String> values, WebServer.HttpInfo info);
  public void setHeaders(Headers responseHeaders);
}
