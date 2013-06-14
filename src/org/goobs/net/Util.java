package org.goobs.net;

import java.io.*;
import java.util.*;

class Util {

	public static final Iterable<File> filesInDirectory(final File dir){
		if(!dir.exists() || !dir.isDirectory() || !dir.canRead()){
			throw new IllegalArgumentException("Bad directory: " + dir);
		}
		return new Iterable<File>(){
			@Override
			public Iterator<File> iterator() {
				final Stack<Pair<File[],Integer>> state = new Stack<Pair<File[], Integer>>();
				state.push(Pair.make(dir.listFiles(), 0));
				return new Iterator<File>(){
					private boolean started = false;
					private File next = null;
					public File tryNext(){
						//(get level)
						Pair<File[],Integer> cand = state.pop();
						File[] files = cand.car();
						int loc = cand.cdr();
						//(get to next valid file)
						while(loc < files.length && (files[loc].getName().equals(".") || files[loc].getName().equals(".."))){
							loc += 1;
						}
						if(loc >= files.length){
							//(listed everything in directory)
							if(state.isEmpty()){
								return null;
							} else {
								return tryNext();
							}
						} else if(files[loc].isDirectory()){
							//(found subdirectory)
							state.push(Pair.make(files,loc+1));
							state.push(Pair.make(files[loc].listFiles(), 0));
							return tryNext();
						} else {
							//(success!)
							state.push(Pair.make(files,loc+1));
							return files[loc];
						}
					}
					@Override
					public boolean hasNext() {
						if(!started){
							next = tryNext();
							started = true;
						}
						return next != null;
					}
					@Override
					public File next() {
						if(!hasNext()){ throw new NoSuchElementException(); }
						File rtn = next;
						next = tryNext();
						return rtn;
					}
					@Override
					public void remove() {
						throw new RuntimeException("NOT IMPLEMENTED");
					}
				};
			}
    };
  }

	public static final String slurp(File file){
		StringBuilder text = new StringBuilder();
		String NL = System.getProperty("line.separator");
		Scanner scanner = null;
		try{
			scanner = new Scanner(new FileInputStream(file));
			while (scanner.hasNextLine()){
				text.append(scanner.nextLine() + NL);
			}
		} catch (FileNotFoundException e) {
			throw new RuntimeException(e);
		} finally{
			if(scanner != null){ scanner.close(); }
		}
		return text.toString();
	}
}
