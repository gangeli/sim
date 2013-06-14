package org.goobs.net;

import java.io.Serializable;
import java.lang.reflect.Type;

public class Pair <E, F> implements Serializable {
	private static final long serialVersionUID = -4684117860320286880L;
	private E e;
	private F f;
	
	public Pair(E e, F f){
		this.e = e;
		this.f = f;
	}
	
	protected Pair(){
		/* for Decodable interface */
		
	}
	
	public E car(){
		return e;
	}
	
	public F cdr(){
		return f;
	}
	
	public void setCar(E car){
		this.e = car;
	}
	
	public void setCdr(F cdr){
		this.f = cdr;
	}
	
	public Object[] toArray(){
		Object[] rtn = new Object[2];
		rtn[0] = car();
		rtn[1] = cdr();
		return rtn;
	}
	
	@SuppressWarnings("unchecked")
	public boolean equals(Object o){
		if(o instanceof Pair){
			return (e == null ? ((Pair) o).e == null : e.equals(((Pair) o).e))
					&& (f == null ? ((Pair) o).f == null : f.equals(((Pair) o).f));
		}
		return false;
	}
	
	
	public int hashCode(){
		return (e == null ? 0 : e.hashCode()) ^ (f == null ? 0 : f.hashCode());
	}
	
	public String toString(){
		StringBuilder b = new StringBuilder();
		b.append("(").append(e).append(" , ").append(f).append(")");
		return b.toString();
	}
	
	
	public static final <E,F> Pair <E,F> make(E car, F cdr){
		return new Pair<E,F>(car, cdr);
	}
}
