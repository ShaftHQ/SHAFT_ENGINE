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
})(jQuery);
