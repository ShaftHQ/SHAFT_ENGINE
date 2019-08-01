package com.shaft.gui.element;

enum JSHelpers {
    LOAD_JQUERY("/** dynamically load jQuery */\n" + "(function(jqueryUrl, callback) {\n"
	    + "    if (typeof jqueryUrl != 'string') {\n"
	    + "        jqueryUrl = 'https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js';\n" + "    }\n"
	    + "    if (typeof jQuery == 'undefined') {\n" + "        var script = document.createElement('script');\n"
	    + "        var head = document.getElementsByTagName('head')[0];\n" + "        var done = false;\n"
	    + "        script.onload = script.onreadystatechange = (function() {\n"
	    + "            if (!done && (!this.readyState || this.readyState == 'loaded'\n"
	    + "                    || this.readyState == 'complete')) {\n" + "                done = true;\n"
	    + "                script.onload = script.onreadystatechange = null;\n"
	    + "                head.removeChild(script);\n" + "                callback();\n" + "            }\n"
	    + "        });\n" + "        script.src = jqueryUrl;\n" + "        head.appendChild(script);\n" + "    }\n"
	    + "    else {\n" + "        callback();\n" + "    }\n"
	    + "})(arguments[0], arguments[arguments.length - 1]);"),
    DRAG_AND_DROP("(function( $ ) {\n" + "        $.fn.simulateDragDrop = function(options) {\n"
	    + "                return this.each(function() {\n"
	    + "                        new $.simulateDragDrop(this, options);\n" + "                });\n"
	    + "        };\n" + "        $.simulateDragDrop = function(elem, options) {\n"
	    + "                this.options = options;\n" + "                this.simulateEvent(elem, options);\n"
	    + "        };\n" + "        $.extend($.simulateDragDrop.prototype, {\n"
	    + "                simulateEvent: function(elem, options) {\n" + "\n"
	    + "                        /*Simulating drag start*/\n"
	    + "                        var type = 'dragstart';\n"
	    + "                        var event = this.createEvent(type);\n"
	    + "                        this.dispatchEvent(elem, type, event);\n" + "\n"
	    + "			/*Simulating drag enter*/\n" + "                        type = 'dragenter';\n"
	    + "                        var dragenterEvent1 = this.createEvent(type, {});\n"
	    + "			dragenterEvent1.dataTransfer = event.dataTransfer;\n"
	    + "                        this.dispatchEvent(elem, type, dragenterEvent1);\n" + "\n"
	    + "			/*Simulating drag over*/\n" + "			type = 'dragover';\n"
	    + "                        var dragoverEvent1 = this.createEvent(type, {});\n"
	    + "                        dragoverEvent1.dataTransfer = event.dataTransfer;\n"
	    + "                        this.dispatchEvent(elem, type, dragoverEvent1);\n" + "\n"
	    + "			/*Simulating drag leave*/\n" + "                        type = 'dragleave';\n"
	    + "                        var dragleaveevent = this.createEvent(type, {});\n"
	    + "			dragleaveevent.dataTransfer = event.dataTransfer;\n"
	    + "                        this.dispatchEvent(elem, type, dragleaveevent);\n" + "\n"
	    + "			/*Sleep for 1000 milliseconds (1 second)*/\n"
	    + "			var start = new Date().getTime();\n"
	    + "			for (var i = 0; i < 1e7; i++) {\n"
	    + "				if ((new Date().getTime() - start) > 1000){\n"
	    + "				break;\n" + "				}\n" + "			}\n"
	    + "\n" + "			/*Simulating drag enter*/\n" + "			type = 'dragenter';\n"
	    + "                        var dragenterEvent = this.createEvent(type, {});\n"
	    + "                        dragenterEvent.dataTransfer = event.dataTransfer;\n"
	    + "                        this.dispatchEvent($(options.dropTarget)[0], type, dragenterEvent);\n" + "\n"
	    + "			/*Simulating drag over*/\n" + "			type = 'dragover';\n"
	    + "                        var dragoverEvent = this.createEvent(type, {});\n"
	    + "                        dragoverEvent.dataTransfer = event.dataTransfer;\n"
	    + "                        this.dispatchEvent($(options.dropTarget)[0], type, dragoverEvent);\n" + "\n"
	    + "                        /*Simulating drop*/\n" + "                        type = 'drop';\n"
	    + "                        var dropEvent = this.createEvent(type, {});\n"
	    + "                        dropEvent.dataTransfer = event.dataTransfer;\n"
	    + "                        this.dispatchEvent($(options.dropTarget)[0], type, dropEvent);\n" + "\n"
	    + "                },\n" + "                createEvent: function(type) {\n"
	    + "                        var event = document.createEvent(\"CustomEvent\");\n"
	    + "                        event.initCustomEvent(type, true, true, null);\n"
	    + "                        event.dataTransfer = {\n" + "                                data: {\n"
	    + "                                },\n" + "                                setData: function(type, val){\n"
	    + "                                        this.data[type] = val;\n"
	    + "                                },\n" + "                                getData: function(type){\n"
	    + "                                        return this.data[type];\n"
	    + "                                }\n" + "                        };\n"
	    + "                        return event;\n" + "                },\n"
	    + "                dispatchEvent: function(elem, type, event) {\n"
	    + "                        if(elem.dispatchEvent) {\n"
	    + "                                elem.dispatchEvent(event);\n"
	    + "                        }else if( elem.fireEvent ) {\n"
	    + "                                elem.fireEvent(\"on\"+type, event);\n" + "                        }\n"
	    + "                }\n" + "        });\n" + "})(jQuery);");

    private String value;

    JSHelpers(String type) {
	this.value = type;
    }

    protected String getValue() {
	return value;
    }
}
