package com.shaft.tools.internal.support;

import lombok.Getter;

@Getter
@SuppressWarnings({"SpellCheckingInspection"})
public enum JavaScriptHelper {
    LOAD_JQUERY("""
            /** dynamically load jQuery */
            (function(jqueryUrl, callback) {
                if (typeof jqueryUrl != 'string') {
                    jqueryUrl = 'https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js';
                }
                if (typeof jQuery == 'undefined') {
                    var script = document.createElement('script');
                    var head = document.getElementsByTagName('head')[0];
                    var done = false;
                    script.onload = script.onreadystatechange = (function() {
                        if (!done && (!this.readyState || this.readyState == 'loaded'
                                || this.readyState == 'complete')) {
                            done = true;
                            script.onload = script.onreadystatechange = null;
                            head.removeChild(script);
                            callback();
                        }
                    });
                    script.src = jqueryUrl;
                    head.appendChild(script);
                }
                else {
                    callback();
                }
            })(arguments[0], arguments[arguments.length - 1]);"""),
    ELEMENT_DRAG_AND_DROP("""
            (function( $ ) {
                    $.fn.simulateDragDrop = function(options) {
                            return this.each(function() {
                                    new $.simulateDragDrop(this, options);
                            });
                    };
                    $.simulateDragDrop = function(elem, options) {
                            this.options = options;
                            this.simulateEvent(elem, options);
                    };
                    $.extend($.simulateDragDrop.prototype, {
                            simulateEvent: function(elem, options) {

                                    /*Simulating drag start*/
                                    var type = 'dragstart';
                                    var event = this.createEvent(type);
                                    this.dispatchEvent(elem, type, event);

            			/*Simulating drag enter*/
                                    type = 'dragenter';
                                    var dragenterEvent1 = this.createEvent(type, {});
            			dragenterEvent1.dataTransfer = event.dataTransfer;
                                    this.dispatchEvent(elem, type, dragenterEvent1);

            			/*Simulating drag over*/
            			type = 'dragover';
                                    var dragoverEvent1 = this.createEvent(type, {});
                                    dragoverEvent1.dataTransfer = event.dataTransfer;
                                    this.dispatchEvent(elem, type, dragoverEvent1);

            			/*Simulating drag leave*/
                                    type = 'dragleave';
                                    var dragleaveevent = this.createEvent(type, {});
            			dragleaveevent.dataTransfer = event.dataTransfer;
                                    this.dispatchEvent(elem, type, dragleaveevent);

            			/*Sleep for 1000 milliseconds (1 second)*/
            			var start = new Date().getTime();
            			for (var i = 0; i < 1e7; i++) {
            				if ((new Date().getTime() - start) > 1000){
            				break;
            				}
            			}

            			/*Simulating drag enter*/
            			type = 'dragenter';
                                    var dragenterEvent = this.createEvent(type, {});
                                    dragenterEvent.dataTransfer = event.dataTransfer;
                                    this.dispatchEvent($(options.dropTarget)[0], type, dragenterEvent);

            			/*Simulating drag over*/
            			type = 'dragover';
                                    var dragoverEvent = this.createEvent(type, {});
                                    dragoverEvent.dataTransfer = event.dataTransfer;
                                    this.dispatchEvent($(options.dropTarget)[0], type, dragoverEvent);

                                    /*Simulating drop*/
                                    type = 'drop';
                                    var dropEvent = this.createEvent(type, {});
                                    dropEvent.dataTransfer = event.dataTransfer;
                                    this.dispatchEvent($(options.dropTarget)[0], type, dropEvent);

                            },
                            createEvent: function(type) {
                                    var event = document.createEvent("CustomEvent");
                                    event.initCustomEvent(type, true, true, null);
                                    event.dataTransfer = {
                                            data: {
                                            },
                                            setData: function(type, val){
                                                    this.data[type] = val;
                                            },
                                            getData: function(type){
                                                    return this.data[type];
                                            }
                                    };
                                    return event;
                            },
                            dispatchEvent: function(elem, type, event) {
                                    if(elem.dispatchEvent) {
                                            elem.dispatchEvent(event);
                                    }else if( elem.fireEvent ) {
                                            elem.fireEvent("on"+type, event);
                                    }
                            }
                    });
            })(jQuery);"""),
    ELEMENT_GET_XPATH("""
            function getXPath(element) {
                var xpath = '';
                var count = 0;
                while (element) {

                    /** Getting the Element's Index
                     **/
                    var pathIndex = "";
                    if ($$GetIndex$$) {
                        try {
                            var index = 0;
                            for (var sibling = element.previousSibling; sibling; sibling = sibling.previousSibling) {
                                if (sibling.nodeType == Node.DOCUMENT_TYPE_NODE)
                                    continue;

                                if (sibling.nodeName == element.nodeName)
                                    ++index;
                            }
                            var pathIndex = (index ? "[" + (index + 1) + "]" : "[1]");
                        } catch (err) {
                            continue;
                        }
                    }

                    /** Getting the Element's Xpath
                     **/
                    var nodeXpath = '';

                    /** Try to get element ID
                     **/
                    try {
                        if (element.id && $$GetId$$) {
                            nodeXpath += '@id=\\"' + element.id + '\\"';
                        }
                    } catch (err) {}

                    /** Try to get element Name
                     **/
                    try {
                        if (element.name && $$GetName$$) {
                            if (nodeXpath != '')
                                nodeXpath += ' and ';
                            nodeXpath += '@name=\\"' + element.name + '\\"';
                        }
                    } catch (err) {}

                    /** Try to get element Type
                     **/
                    try {
                        if (element.hasAttribute("type") && typeof element.type !== 'undefined' && $$GetType$$) {
                            if (nodeXpath != '')
                                nodeXpath += ' and ';
                            nodeXpath += '@type=\\"' + element.type + '\\"';
                        }
                    } catch (err) {}

                    /** Try to get element Class Name
                     **/
                    try {
                        if (element.className && nodeXpath == '' && $$GetClass$$) {
                            if (nodeXpath != '')
                                nodeXpath += ' and ';
                            nodeXpath += '@class=\\"' + element.className + '\\"';
                        }
                    } catch (err) {}

                    /** Try to get element Text
                     **/
                    try {
                        if (element.textContent && element.textContent.length < 50 && element.textContent == element.innerHTML && $$GetText$$) {
                            var uiElementText = element.textContent;
                            try {
                                uiElementText = uiElementText.trim();
                            } catch (err) {
                                uiElementText = uiElementText.replace(/^\\s+|\\s+$/g, '');
                            }
                            if (nodeXpath != '')
                                nodeXpath += ' and ';
                            if (element.textContent == uiElementText && element.textContent.length > 0)
                                nodeXpath += 'text()=\\"' + element.textContent + '\\"';
                            else
                                nodeXpath += 'normalize-space() = \\"' + uiElementText + '\\"';
                        } else if (element.text && element.text.length < 50 && element.text == element.innerHTML && $$GetText$$) {
                            var uiElementText = element.text;
                            try {
                                uiElementText = uiElementText.trim();
                            } catch (err) {
                                uiElementText = uiElementText.replace(/^\\s+|\\s+$/g, '');
                            }
                            if (uiElementText.length > 0) {
                                if (nodeXpath != '')
                                    nodeXpath += ' and ';
                                if (element.text == uiElementText)
                                    nodeXpath += 'contains(text(),\\'' + uiElementText + '\\')';
                                else
                                    nodeXpath += 'contains(normalize-space(),\\'' + uiElementText + '\\')';
                            }
                        } else if (element.innerText && element.innerText.length < 50 && element.innerText == element.innerHTML && $$GetText$$) {

                            var uiElementText = element.innerText;
                            try {
                                uiElementText = uiElementText.trim();
                            } catch (err) {
                                uiElementText = uiElementText.replace(/^\\s+|\\s+$/g, '');
                            }
                            if (uiElementText.length > 0) {
                                if (nodeXpath != '')
                                    nodeXpath += ' and ';
                                if (element.innerText == uiElementText)
                                    nodeXpath += 'contains(text(),\\'' + uiElementText + '\\')';
                                else
                                    nodeXpath += 'contains(normalize-space(),\\'' + uiElementText + '\\')';
                            }
                        } else if (element.nodeName.toLocaleLowerCase() == "a" || count == 0) {
                            var uiElementText = element.textContent;
                            try {
                                uiElementText = uiElementText.trim().substring(0, 10);
                                uiElementText = uiElementText.replace("'", "') and contains (.,'");
                            } catch (err) {
                                uiElementText = uiElementText.replace(/^\\s+|\\s+$/g, '');
                                uiElementText = uiElementText.replace("'", "') and contains (.,'");
                            }
                            if (uiElementText.length > 0) {
                                if (nodeXpath != '')
                                    nodeXpath += ' and ';
                                if (element.textContent == uiElementText)
                                    nodeXpath += 'contains(normalize-space(),\\'' + uiElementText + '\\')';
                                else
                                    nodeXpath += 'contains(.,\\'' + uiElementText + '\\')';
                            }
                        }
                    } catch (err) {}

                    /** Getting the Element's Tag Name
                     **/
                    var currentElementTagName = element.nodeName.toLocaleLowerCase();

                    /** Building Xpath for the current Element Node
                     **/
                    if (nodeXpath == '') {
                        xpath = '/' + currentElementTagName + pathIndex + xpath;
                    } else {
                        xpath = '/' + currentElementTagName + pathIndex + '[' + nodeXpath + ']' + xpath;
                    }

                    /** Switching focus to parent node
                     **/
                    element = element.parentElement;

                    /** Incrementing the element counter and breaking the loop in case we reach the maximum number of elements defined by the user
                     **/
                    count++;
                    if (count >= $$MaxCount$$)
                        break;
                }

                return '/' + xpath;
            };

            if (arguments[0] && !window.lastelem) {
                return getXPath(arguments[0]);
            } else if (!arguments[0] && window.lastelem) {
                window.lastelem.style.outline = currentoutlineStyle;
                window.lastelem.style.backgroundColor = currentbackgroundColorStyle;
                return getXPath(window.lastelem);
            }"""),
    ELEMENT_SCROLL_TO_VIEWPORT(
            "(function(){'use strict';var api;api=function(x,y){var elm,scrollX,scrollY,newX,newY;scrollX=window.pageXOffset;scrollY=window.pageYOffset;window.scrollTo(x,y);newX=x-window.pageXOffset;newY=y-window.pageYOffset;elm=this.elementFromPoint(newX,newY);window.scrollTo(scrollX,scrollY);return elm;};this.document.elementFromAbsolutePoint=api;}).call(this);return document.elementFromAbsolutePoint(arguments[0], arguments[1]);"),
    WINDOW_FOCUS("window.focus();"), WINDOW_RESET_LOCATION("window.moveTo(0,0);"),
    WINDOW_RESIZE("window.resizeTo($WIDTH,$HEIGHT);"),
    DOCUMENT_READY_STATE("return document.readyState;"),
    JQUERY_ACTIVE_STATE("return jQuery.active;"),
    ANGULAR_READY_STATE("return angular.element(document).injector().get('$http').pendingRequests.length;");

    private final String value;

    JavaScriptHelper(String type) {
        this.value = type;
    }

}
