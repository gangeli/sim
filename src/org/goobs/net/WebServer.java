package org.goobs.net;

import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.*;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public class WebServer {
	
	private static final String ICON_REGEX = "/favicon\\.ico";
	
	private HttpServer server;
	private int port;
	private byte[] icon;

	public static final class HttpInfo {
		public HashMap <String,List<String>> headers = new HashMap<String,List<String>>();
	}


	public WebServer(int port){
		this.port = port;
	}

	public WebServer start(){
		try {
			server = HttpServer.create(new InetSocketAddress(port), 0);
			server.start();
		} catch (IOException e) {
			throw new RuntimeException("Could not start web server: " + e.getMessage());
		}
    return this;
	}

	public WebServer register(final String uri, final WebServerHandler handler){
		if(server == null){
			throw new IllegalStateException("Start the server before registering listeners");
		}
		server.createContext(uri, new HttpHandler(){
			@Override
			public void handle(HttpExchange exchange) throws IOException {
				try {
          OutputStream responseBody = exchange.getResponseBody();
          HashMap <String, String> values = new HashMap<String, String>();
					if(exchange.getRequestMethod().equalsIgnoreCase("GET")){
						//--Custom Icon
						if(exchange.getRequestURI().getPath().matches(ICON_REGEX)){
							exchange.getResponseHeaders().set("Content-Type", "image/x-icon");
							exchange.sendResponseHeaders(200, 0);
							responseBody.write(getIcon());
							responseBody.close();
							return;
						}
            //--Create Values Map
						String query = exchange.getRequestURI().getQuery();
						values = parseQuery(query);
          } else if (exchange.getRequestMethod().equalsIgnoreCase("POST")) {
            StringBuilder b = new StringBuilder();
            BufferedInputStream request = new BufferedInputStream(exchange.getRequestBody());
            while(request.available() > 0) {
              b.append((char) request.read());
            }
            values.put("data", b.toString());
          } else {
						System.err.println("[WebServer]: Warning: unhandled request of type: " + exchange.getRequestMethod());
            return;
					}
          //--Create Info
          HttpInfo info = new HttpInfo();
          Headers responseHeaders = exchange.getResponseHeaders();
          responseHeaders.set("Content-Type", "text/html");
          handler.setHeaders(responseHeaders);
          exchange.sendResponseHeaders(200, 0);
          Headers requestHeaders = exchange.getRequestHeaders();
          Set<String> keySet = requestHeaders.keySet();
          Iterator<String> iter = keySet.iterator();
          HashMap <String,List<String>> headers = new HashMap<String,List<String>>();
          while (iter.hasNext()) {
            String key = iter.next();
            List <String> vals = requestHeaders.get(key);
            headers.put(key, vals);
          }
          info.headers = headers;
          responseBody.write( handler.handle(values, info).getBytes() );
          responseBody.close();
				} catch(Exception e){
					e.printStackTrace();
				}
			}
		});
    return this;
	}
	
	public WebServer mount(String uri, final File directory){
		while(uri.endsWith("/")){
			uri = uri.substring(0, uri.length()-1);
		}
		for(File toMount : Util.filesInDirectory(directory)){
			String uriPath = uri + toMount.getPath().substring(directory.getPath().length());
			FileHandler handler = new FileHandler(toMount);
			register(uriPath, handler);
			if(toMount.getName().equals("index.html")){
				register(uriPath.substring(0,uriPath.length()-10), handler);
			}
		}
		return this;
	}

	public void setIcon(byte[] icon){
		this.icon = icon;
	}
	
	private byte[] getIcon(){
		if(icon == null){
			return "".getBytes();
		}else{
			return icon;
		}
	}

	
	private static HashMap<String, String> parseQuery(String query) {
		HashMap <String,String> rtn = new HashMap <String,String>();
		if(query == null) return rtn;
		String[] vals = query.split("&");
		for(String val : vals){ 
			String[] v = val.split("=");
			if(v.length != 2){ throw new IllegalArgumentException("Invalid query part: " + val); }
			rtn.put(v[0], v[1]);
		}
		return rtn;
	}

	public static void main(String[] args) throws IOException {
		WebServer server = new WebServer(4242);
		server.start(); // Must be called before registering handlers
		server.register("/", new SimpleHandler(){ // see also JSON handler (for JQuery callbacks), or FileHandler (for simple html)
			@Override
			public String handle(HashMap<String, String> values, HttpInfo info) {
        // "values" are the parameters (e.g., "/foo?x=7&y=9" would create a map { x -> 7, y -> 9 }
				StringBuilder response = new StringBuilder("<html>Received: <br>&nbsp;&nbsp;&nbsp;" );
				for(String key : values.keySet()){
					response.append(key).append("&rarr;").append(values.get(key)).append("<br>&nbsp;&nbsp;&nbsp;");
				}
				return response.append("</html>").toString(); // Return a simple String. JsonHandler may be useful for
                                                      // JSON (e.g., JQuery) callbacks
			}
    });
		
    
    server.register("/foo", new FileHandler("/home/gabor/workspace/web/foo.html"));
		System.out.println("Started a webserver on port 4242");
	}
}
