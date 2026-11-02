function __shaftAriaSnapshot(root) {
    var IMPLICIT_ROLES_BY_TAG = {
        a: 'link',
        button: 'button',
        h1: 'heading', h2: 'heading', h3: 'heading', h4: 'heading', h5: 'heading', h6: 'heading',
        nav: 'navigation',
        ul: 'list', ol: 'list',
        li: 'listitem',
        img: 'img',
        main: 'main',
        header: 'banner',
        footer: 'contentinfo',
        table: 'table',
        form: 'form',
        textarea: 'textbox',
        option: 'option',
        dialog: 'dialog',
        article: 'article',
        section: 'region'
    };

    var IMPLICIT_INPUT_ROLES_BY_TYPE = {
        text: 'textbox', email: 'textbox', search: 'searchbox', tel: 'textbox', url: 'textbox', password: 'textbox',
        checkbox: 'checkbox', radio: 'radio', range: 'slider',
        button: 'button', submit: 'button', reset: 'button',
        number: 'spinbutton'
    };

    function computeRole(el) {
        var explicit = el.getAttribute('role');
        if (explicit) {
            return explicit.split(/\s+/)[0];
        }
        var tag = el.tagName.toLowerCase();
        if (tag === 'a') {
            return el.hasAttribute('href') ? 'link' : null;
        }
        if (tag === 'input') {
            var type = (el.getAttribute('type') || 'text').toLowerCase();
            return IMPLICIT_INPUT_ROLES_BY_TYPE[type] || 'textbox';
        }
        if (tag === 'select') {
            return el.multiple ? 'listbox' : 'combobox';
        }
        return IMPLICIT_ROLES_BY_TAG[tag] || null;
    }

    function resolveLabelledBy(ids) {
        return ids.split(/\s+/).map(function (id) {
            var referenced = document.getElementById(id);
            return referenced ? referenced.textContent : '';
        }).join(' ').trim();
    }

    function directText(el) {
        var text = '';
        for (var i = 0; i < el.childNodes.length; i++) {
            var node = el.childNodes[i];
            if (node.nodeType === 3) {
                text += node.textContent;
            }
        }
        return text.replace(/\s+/g, ' ').trim();
    }

    function computeName(el) {
        var ariaLabel = el.getAttribute('aria-label');
        if (ariaLabel && ariaLabel.trim()) {
            return ariaLabel.trim();
        }
        var labelledBy = el.getAttribute('aria-labelledby');
        if (labelledBy) {
            var resolved = resolveLabelledBy(labelledBy);
            if (resolved) {
                return resolved;
            }
        }
        if (el.tagName.toLowerCase() === 'img') {
            var alt = el.getAttribute('alt');
            if (alt) {
                return alt.trim();
            }
        }
        var title = el.getAttribute('title');
        if (title && title.trim()) {
            return title.trim();
        }
        if ('value' in el && typeof el.value === 'string' && el.value.trim()) {
            return el.value.trim();
        }
        return directText(el);
    }

    function isHidden(el) {
        if (el.getAttribute('aria-hidden') === 'true') {
            return true;
        }
        if (el.hidden) {
            return true;
        }
        var style = window.getComputedStyle(el);
        return style.display === 'none' || style.visibility === 'hidden';
    }

    function walk(el) {
        if (!el || el.nodeType !== 1 || isHidden(el)) {
            return [];
        }
        var children = [];
        for (var i = 0; i < el.children.length; i++) {
            children = children.concat(walk(el.children[i]));
        }
        var role = computeRole(el);
        if (role) {
            return [{role: role, name: computeName(el), children: children}];
        }
        return children;
    }

    return walk(root);
}
