package klojure.java.deps;

import clojure.lang.RT;
import java.util.Collections;
import java.util.List;
import org.apache.karaf.shell.api.action.Action;
import org.apache.karaf.shell.api.action.Argument;
import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.Option;
import org.apache.karaf.shell.api.action.lifecycle.Service;

@Command(
    scope = "deps",
    name = "generate",
    description =
        "Given a path to a DDF source directory, generates dependency data for use in "
            + "visualizing the OSGi container. Ensure the repo matches the version of DDF currently running.")
@Service
public class DepsGenerate extends AbstractKlojureCommand implements Action {

  @Argument(
      name = "pathToDdfSource",
      description = "Absolute path to the root pom of your DDF repo.",
      required = true)
  private String pathToDdfSource;

  @Option(
      name = "-l",
      aliases = {"--log"},
      description = "Increases log level of wait information.",
      required = false,
      multiValued = false)
  boolean increaseLogLevel = false;

  @Override
  public List<String> getNamespaces() throws Exception {
    return Collections.singletonList("rebel-readline.main");
  }

  @Override
  public void invokeRuntime() throws Exception {
    RT.var("rebel-readline.main", "-main").invoke();
  }
}
