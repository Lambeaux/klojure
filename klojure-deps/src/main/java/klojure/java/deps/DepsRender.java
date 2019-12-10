package klojure.java.deps;

import clojure.lang.RT;
import java.util.Collections;
import java.util.List;
import org.apache.karaf.shell.api.action.Action;
import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.Option;
import org.apache.karaf.shell.api.action.lifecycle.Service;

@Command(
    scope = "deps",
    name = "render",
    description =
        "Exports a dependency graph that can be opened in a web browser to DDF_HOME/graphs/viz.html, "
            + "which conforms to any filters or settings in the command.")
@Service
public class DepsRender extends AbstractKlojureCommand implements Action {

  @Option(
      name = "-t",
      aliases = {"--third-party"},
      description =
          "Includes third party dependencies in the graph output when set to true. Defaults to false.",
      required = false,
      multiValued = false)
  boolean includeThirdPartyDependencies = false;

  @Override
  public List<String> getNamespaces() throws Exception {
    return Collections.singletonList("rebel-readline.main");
  }

  @Override
  public void invokeRuntime() throws Exception {
    RT.var("rebel-readline.main", "-main").invoke();
  }
}
