package org.goobs.net;

import com.sun.net.httpserver.Headers;

/**
 * @author Gabor Angeli (angeli at cs.stanford)
 */
public abstract class SimpleHandler implements WebServerHandler {
  @Override
  public void setHeaders(Headers responseHeaders) {
		responseHeaders.set("Content-Type", "text/html");
	}
}
