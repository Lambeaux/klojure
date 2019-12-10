package klojure.java.deps;

import clojure.lang.RT;
import clojure.lang.Symbol;
import java.lang.reflect.Field;
import java.util.Hashtable;
import java.util.List;
import org.apache.karaf.shell.api.action.Action;
import sun.misc.Signal;

public abstract class AbstractKlojureCommand implements Action {

  @Override
  public Object execute() throws Exception {

    Field signals = Signal.class.getDeclaredField("signals");
    signals.setAccessible(true);
    Hashtable signalsSave = (Hashtable) ((Hashtable) signals.get(null)).clone();

    Field handlers = Signal.class.getDeclaredField("handlers");
    handlers.setAccessible(true);
    Hashtable handlersSave = (Hashtable) ((Hashtable) handlers.get(null)).clone();

    Thread.currentThread().setContextClassLoader(AbstractKlojureCommand.class.getClassLoader());

    this.getNamespaces().forEach(ns -> RT.var("clojure.core", "require").invoke(Symbol.intern(ns)));

    try {
      invokeRuntime();
    } finally {
      signals.set(null, signalsSave);
      handlers.set(null, handlersSave);
    }

    return null;
  }

  public abstract List<String> getNamespaces() throws Exception;

  public abstract void invokeRuntime() throws Exception;
}
