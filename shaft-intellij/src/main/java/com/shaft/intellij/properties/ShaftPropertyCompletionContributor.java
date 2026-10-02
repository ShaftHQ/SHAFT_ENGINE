package com.shaft.intellij.properties;

import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.lang.properties.parsing.PropertiesTokenTypes;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiUtilCore;
import org.jetbrains.annotations.NotNull;

/** Completes SHAFT property keys, showing each default and type (issue #6417). */
public final class ShaftPropertyCompletionContributor extends CompletionContributor {
    @Override
    public void fillCompletionVariants(@NotNull CompletionParameters parameters, @NotNull CompletionResultSet result) {
        PsiElement position = parameters.getPosition();
        if (PsiUtilCore.getElementType(position) != PropertiesTokenTypes.KEY_CHARACTERS
                || !ShaftPropertiesFiles.applies(parameters.getOriginalFile())) {
            return;
        }
        for (ShaftProperty property : ShaftPropertyCatalog.all()) {
            result.addElement(LookupElementBuilder.create(property.key())
                    .withTypeText(property.defaultValue().isEmpty() ? property.type() : "= " + property.defaultValue())
                    .withTailText("  " + property.group(), true));
        }
    }
}
