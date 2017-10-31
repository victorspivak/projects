package svl.learn.java.script;

import jdk.nashorn.api.scripting.ScriptObjectMirror;

import javax.script.*;
import java.util.Optional;

/**
 * ScriptSample
 *
 * @author victor.spivak
 * @since 212
 */
public class ScriptSample {
    private ThreadLocal<Bindings> bindingsProvider;
    private Compilable compilableEngine;
    private CompiledScript compiledScript;

    public static void main(String[] args) throws ScriptException {
        ScriptSample sample = new ScriptSample();
        sample.test();
        sample.testCompiled();
    }

    public ScriptSample() throws ScriptException {
        ScriptEngineManager engineManager = new ScriptEngineManager();
        ScriptEngine engine = engineManager.getEngineByName("nashorn");
        if (engine instanceof Compilable) {
            compilableEngine = (Compilable) engine;
            compiledScript = compilableEngine.compile("greeting = 'Hello ' + name;");
        }

        bindingsProvider = ThreadLocal.withInitial(engine::createBindings);
    }

    private void test() throws ScriptException {
        ScriptEngineManager engineManager = new ScriptEngineManager();

        ScriptEngine engine1 = engineManager.getEngineByName("nashorn");

        engine1.eval("function sum(a, b) { return a + b; }");
        System.out.println(engine1.eval("sum(1, 2);"));
        System.out.println(engine1.eval("sum(5, 2);"));

        engine1.eval("function sub(a, b) { return a - b; }");
        System.out.println(engine1.eval("sub(5, 2);"));
        System.out.println(engine1.eval("sum(11, 2);"));


        ScriptEngine engine2 = engineManager.getEngineByName("nashorn");
        engine2.eval("function mul(a, b) { return a * b; }");
        engine2.eval("function sum(a, b) { return 2 * (a + b); }");
        System.out.println(engine2.eval("mul(5, 2);"));
        System.out.println(engine2.eval("sum(5, 2);"));

        System.out.println(engine1.eval("sum(10, 2);"));

        ScriptEngine engine3 = engineManager.getEngineByName("nashorn");
        engine3.put("name", "Victor");
        engine3.eval("print(\"Hello \" + name)");
        engine3.put("name", "Jack");
        engine3.eval("res = \"Hello \" + name");
        System.out.println("res = " + engine3.get("res"));
        System.out.println("engine3.getClass() = " + engine3.get("res").getClass());
        engine3.eval("res = null");
        Object res = Optional.ofNullable(engine3.get("res"));
        System.out.println("res = " + res);

        engine3.eval("res = ['a', 'b', 'ccc']");
        Object res1 = engine3.get("res");
        System.out.println("res = " + res1.getClass());
        if (res1 instanceof ScriptObjectMirror) {
            ScriptObjectMirror objectMirror = (ScriptObjectMirror) res1;
            System.out.println("objectMirror.isArray() = " + objectMirror.isArray());
            objectMirror.values().forEach(System.out::println);
        }
    }

    private void testCompiled() throws ScriptException {
        System.out.println("================================================");
        Bindings bindings = this.bindingsProvider.get();
        bindings.put("name", "Victor");
        compiledScript.eval(bindings);
        System.out.println("result = " + bindings.get("greeting"));
    }
}
