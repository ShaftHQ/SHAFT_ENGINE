package com.shaft.api;

import io.restassured.response.Response;
import org.testng.Assert;
import org.testng.annotations.Test;

import javax.tools.ToolProvider;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

class RestActionsBytecodeCompatibilityTest {
    @Test
    void jsonListMethodShouldVerifyWhenRuntimeJsonExceptionIsChecked() throws Exception {
        Path temp = Files.createTempDirectory("shaft-rest-actions-verifier-");
        try {
            Path source = temp.resolve("src/org/json/JSONException.java");
            Path classes = temp.resolve("classes");
            Files.createDirectories(source.getParent());
            Files.createDirectories(classes);
            Files.writeString(source, """
                    package org.json;
                    public class JSONException extends Exception {
                        public JSONException(String message) { super(message); }
                    }
                    """, StandardCharsets.UTF_8);
            int compileResult = ToolProvider.getSystemJavaCompiler().run(null, null, null,
                    "-d", classes.toString(), source.toString());
            Assert.assertEquals(compileResult, 0, "Checked JSONException compatibility fixture must compile");

            Path productionClasses = Path.of("target", "classes");
            ClassLoader loader = new ExactShadowingLoader(getClass().getClassLoader(), productionClasses, classes);

            Class<?> restActions = Class.forName("com.shaft.api.RestActions", true, loader);
            var method = restActions.getDeclaredMethod("getResponseJSONValueAsList", Response.class, String.class);

            Assert.assertTrue(Modifier.isPublic(method.getModifiers()));
            Assert.assertTrue(Modifier.isStatic(method.getModifiers()));
            Assert.assertEquals(method.getReturnType(), List.class);
        } finally {
            try (var paths = Files.walk(temp)) {
                for (Path path : paths.sorted(java.util.Comparator.reverseOrder()).toList()) {
                    Files.deleteIfExists(path);
                }
            }
        }
    }

    private static final class ExactShadowingLoader extends ClassLoader {
        private final Path productionClasses;
        private final Path shadowClasses;

        private ExactShadowingLoader(ClassLoader parent, Path productionClasses, Path shadowClasses) {
            super(parent);
            this.productionClasses = productionClasses;
            this.shadowClasses = shadowClasses;
        }

        @Override
        protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
            synchronized (getClassLoadingLock(name)) {
                Class<?> loaded = findLoadedClass(name);
                if (loaded == null && ("com.shaft.api.RestActions".equals(name) || "org.json.JSONException".equals(name))) {
                    loaded = defineExactClass(name);
                }
                if (loaded == null) {
                    loaded = super.loadClass(name, false);
                }
                if (resolve) {
                    resolveClass(loaded);
                }
                return loaded;
            }
        }

        private Class<?> defineExactClass(String name) throws ClassNotFoundException {
            Path root = "org.json.JSONException".equals(name) ? shadowClasses : productionClasses;
            Path classFile = root.resolve(name.replace('.', '/') + ".class");
            try {
                byte[] bytecode = Files.readAllBytes(classFile);
                return defineClass(name, bytecode, 0, bytecode.length);
            } catch (IOException exception) {
                throw new ClassNotFoundException(name, exception);
            }
        }
    }
}
