package testPackage.unitTests;

import com.shaft.driver.SHAFT;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Guards parallel execution (#6396): production classes must not expose new non-final
 * {@code public static} fields, because any thread can reassign them and race other tests.
 */
public class GlobalMutableStateGuardUnitTest {
    /**
     * Justified exceptions. Each entry is assigned once during engine bootstrap and read-only afterwards.
     */
    private static final Set<String> ALLOWLIST = Set.of(
            "com.shaft.properties.internal.Properties#internal",
            "com.shaft.properties.internal.Properties#testNG",
            "com.shaft.properties.internal.Properties#log4j",
            "com.shaft.properties.internal.Properties#cucumber");

    @SuppressWarnings("unused")
    static class LeakyFixture {
        public static String mutable = "x";
        public static final String CONSTANT = "y";
    }

    static List<String> mutablePublicStatics(Class<?> type) {
        List<String> found = new ArrayList<>();
        Field[] fields;
        try {
            fields = type.getDeclaredFields();
        } catch (LinkageError ignored) {
            return found; // optional dependency missing from the test classpath
        }
        for (Field field : fields) {
            int m = field.getModifiers();
            if (Modifier.isPublic(m) && Modifier.isStatic(m) && !Modifier.isFinal(m) && !field.isSynthetic()) {
                found.add(type.getName() + "#" + field.getName());
            }
        }
        return found;
    }

    @Test
    public void detectorFlagsMutablePublicStaticField() {
        Assert.assertEquals(mutablePublicStatics(LeakyFixture.class),
                List.of(LeakyFixture.class.getName() + "#mutable"));
    }

    @Test
    public void productionClassesExposeNoUnapprovedMutablePublicStatics() throws IOException, URISyntaxException {
        Path root = Path.of(SHAFT.class.getProtectionDomain().getCodeSource().getLocation().toURI());
        Assert.assertTrue(Files.isDirectory(root.resolve("com/shaft")), "Expected compiled classes under " + root);
        List<String> violations = new ArrayList<>();
        ClassLoader loader = getClass().getClassLoader();
        try (Stream<Path> files = Files.walk(root.resolve("com/shaft"))) {
            for (Path file : (Iterable<Path>) files.filter(p -> p.toString().endsWith(".class"))::iterator) {
                String name = root.relativize(file).toString().replace('/', '.').replace('\\', '.');
                name = name.substring(0, name.length() - ".class".length());
                try {
                    mutablePublicStatics(Class.forName(name, false, loader)).stream()
                            .filter(f -> !ALLOWLIST.contains(f)).forEach(violations::add);
                } catch (ClassNotFoundException | LinkageError ignored) {
                    // optional dependency missing from the test classpath
                }
            }
        }
        Assert.assertTrue(violations.isEmpty(), "Non-final public static fields break parallel isolation; make them final, "
                + "thread-local, or private with accessors (or justify in ALLOWLIST): " + violations);
    }
}
