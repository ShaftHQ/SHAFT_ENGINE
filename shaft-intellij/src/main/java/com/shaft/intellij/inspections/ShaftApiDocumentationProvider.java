package com.shaft.intellij.inspections;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiMember;
import org.jetbrains.annotations.Nullable;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Adds a SHAFT user-guide link (Shift+F1, and the external-doc link in Ctrl+Q) for
 * {@code com.shaft} classes and members, next to their Javadoc (issue #6418).
 */
public final class ShaftApiDocumentationProvider extends AbstractDocumentationProvider {
    static final String SEARCH_URL = "https://shafthq.github.io/search?q=";

    @Override
    public @Nullable List<String> getUrlFor(PsiElement element, PsiElement originalElement) {
        String query = query(element);
        return query == null ? null : List.of(SEARCH_URL + URLEncoder.encode(query, StandardCharsets.UTF_8));
    }

    static @Nullable String query(@Nullable PsiElement element) {
        PsiClass owner = element instanceof PsiClass type ? type
                : element instanceof PsiMember member ? member.getContainingClass() : null;
        String qualified = owner == null ? null : owner.getQualifiedName();
        if (qualified == null || !qualified.startsWith("com.shaft.")) {
            return null;
        }
        return element instanceof PsiClass ? owner.getName() : owner.getName() + "." + ((PsiMember) element).getName();
    }
}
