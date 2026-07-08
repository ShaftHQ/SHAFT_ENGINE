package com.shaft.gui.internal.aria;

import java.util.List;

/**
 * A single accessible-name-tree node: an ARIA role, its accessible name, and its children.
 *
 * @param role the ARIA role (explicit {@code role} attribute or implicit-by-tag-name)
 * @param name the accessible name, resolved via aria-label/aria-labelledby/alt/title/value/text, or empty
 * @param children the node's own accessible descendants (never {@code null})
 */
public record AriaNode(String role, String name, List<AriaNode> children) {
    public AriaNode {
        name = name == null ? "" : name;
        children = children == null ? List.of() : List.copyOf(children);
    }
}
