package com.shaft.tools.io.internal;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

public class TraceViewerHtmlLineEndingTest {
    @DataProvider
    public Object[][] lineEndings() {
        return new Object[][]{{"\n"}, {"\r\n"}};
    }

    @Test(dataProvider = "lineEndings")
    public void viewerScriptsAreInlinedWithEitherCheckoutLineEnding(String lineEnding) throws Exception {
        ClassLoader parent = TraceViewerHtml.class.getClassLoader();
        String resourceName = "META-INF/shaft/trace-viewer/index.html";
        byte[] template;
        try (InputStream input = parent.getResourceAsStream(resourceName)) {
            Assert.assertNotNull(input);
            template = new String(input.readAllBytes(), StandardCharsets.UTF_8)
                    .replace("\r\n", "\n").replace("\n", lineEnding).getBytes(StandardCharsets.UTF_8);
        }
        // Isolate the renderer's template cache and resource input without mutating shared resources.
        ClassLoader loader = new ClassLoader(parent) {
            @Override
            public InputStream getResourceAsStream(String name) {
                return resourceName.equals(name) ? new ByteArrayInputStream(template) : super.getResourceAsStream(name);
            }

            @Override
            protected synchronized Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
                if (!name.equals(TraceViewerHtml.class.getName()) && !name.equals(JsonEscapes.class.getName())) {
                    return super.loadClass(name, resolve);
                }
                Class<?> loaded = findLoadedClass(name);
                if (loaded == null) {
                    try (InputStream input = parent.getResourceAsStream(name.replace('.', '/') + ".class")) {
                        if (input == null) {
                            throw new ClassNotFoundException(name);
                        }
                        byte[] bytes = input.readAllBytes();
                        loaded = defineClass(name, bytes, 0, bytes.length);
                    } catch (IOException exception) {
                        throw new ClassNotFoundException(name, exception);
                    }
                }
                if (resolve) {
                    resolveClass(loaded);
                }
                return loaded;
            }
        };
        var renderer = loader.loadClass(TraceViewerHtml.class.getName());
        var render = renderer.getDeclaredMethod("render", String.class, List.class);
        render.setAccessible(true);
        String html = (String) render.invoke(null, "{}", List.of());
        Assert.assertTrue(html.contains("function snapshotDocument(html)"), "The executable viewer must be embedded.");
        Assert.assertFalse(html.contains("/*@VIEWER_CORE_JS@*/"), "Core script placeholder was not replaced.");
        Assert.assertFalse(html.contains("/*@VIEWER_JS@*/"), "Viewer script placeholder was not replaced.");
    }
}
