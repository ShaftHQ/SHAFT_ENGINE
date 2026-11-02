package com.shaft.intellij.properties;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.lang.properties.psi.Property;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/** Quick documentation (Ctrl+Q) for SHAFT property keys (issue #6418). */
public final class ShaftPropertyDocumentationProvider extends AbstractDocumentationProvider {
    @Override
    public @Nullable String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
        ShaftProperty property = property(element);
        return property == null ? null : render(property);
    }

    @Override
    public @Nullable List<String> getUrlFor(PsiElement element, PsiElement originalElement) {
        return property(element) == null ? null : List.of(ShaftPropertyCatalog.USER_GUIDE_URL);
    }

    static String render(ShaftProperty property) {
        StringBuilder html = new StringBuilder("<div class='definition'><pre><b>")
                .append(StringUtil.escapeXmlEntities(property.key())).append("</b> : ")
                .append(StringUtil.escapeXmlEntities(property.type())).append("</pre></div><div class='content'>");
        if (!property.description().isEmpty()) {
            html.append(StringUtil.escapeXmlEntities(property.description()));
        }
        html.append("</div><table class='sections'>")
                .append("<tr><td class='section'>Default:</td><td><code>")
                .append(property.defaultValue().isEmpty() ? "none" : StringUtil.escapeXmlEntities(property.defaultValue()))
                .append("</code></td></tr><tr><td class='section'>Group:</td><td>")
                .append(StringUtil.escapeXmlEntities(property.group()))
                .append("</td></tr><tr><td class='section'>User guide:</td><td><a href='")
                .append(ShaftPropertyCatalog.USER_GUIDE_URL).append("'>Properties reference</a></td></tr></table>");
        return html.toString();
    }

    private static @Nullable ShaftProperty property(@Nullable PsiElement element) {
        if (element instanceof Property property && property.getUnescapedKey() != null) {
            return ShaftPropertyCatalog.find(property.getUnescapedKey());
        }
        return null;
    }
}
