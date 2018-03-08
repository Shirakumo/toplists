var Toplists = function(list){
    var self = this;
    list = list || document.getElementById("list");
    var images = list.querySelector(".images");
    var items = list.querySelector(".items");

    self.idIndex = (root, id)=>{
        var i = 0;
        for(var el of root.children){
            if(el.dataset.id == id) return i;
            i++;
        }
        return -1;
    };

    self.sortChildren = (container, score)=>{
        const items = [...container.children];

        items.sort((a, b) => score(a) - score(b))
            .forEach(item => container.appendChild(item));
        return container;
    };

    self.saveOrder = ()=>{
        var order = [];
        for(var el of images.children){
            order.push(el.dataset.id);
        }
        window.localStorage.setItem("order", JSON.stringify(order));
        return order;
    };

    self.loadOrder = ()=>{
        var order = JSON.parse(window.localStorage.getItem("order")||"[]");
        self.sortChildren(images, (el)=> order.indexOf(el.dataset.id));
        self.sortChildren(items, (el)=> order.indexOf(el.dataset.id));
        return images;
    };

    self.imageForSorter = (sorter) =>{
        return images.children[self.idIndex(images, sorter.dataset.id)];
    };

    self.makeImageActive = (image) =>{
        if(!image.classList.contains("active")){
            [].slice.call(images.children).forEach((img)=>img.classList.remove("active"));
            image.classList.add("active");
        }
        return image;
    }

    self.makeDraggable = (root, onBegin, onMove, onFinish)=>{
        var drag = null;
        var onStart = null;
        var onDrag = null;
        var onEnd = null;
        var onHover = null;
        var onUnhover = null;

        onStart = (ev)=>{
            drag = ev.target;
            ev.dataTransfer.effectAllowed = "move";
            setTimeout(()=>{drag.classList.add("ghost");}, 0);
            if(onBegin) onBegin(drag);
            root.removeEventListener("mouseover", onHover);
            root.removeEventListener("mouseout", onUnhover);
        };

        onDrag = (ev)=>{
            if(!drag) return;
            ev.preventDefault();
            ev.dataTransfer.dropEffect = "move";
            var target = ev.target;
            if(target && target !== drag){
                if(target.parentNode == root)
                    root.insertBefore(drag, target);
                else
                    root.appendChild(drag);
            }
            if(onMove) onMove(drag);
        };

        onEnd = (ev)=>{
            ev.preventDefault();
            drag.classList.remove("ghost");
            if(onFinish) onFinish(drag);
            drag = null;
            root.addEventListener("mouseover", onHover, false);
            root.addEventListener("mouseout", onUnhover, false);
        };

        onHover = (ev)=>{
            if(onBegin && ev.target.parentNode == root)
                onBegin(ev.target);
        };

        onUnhover = (ev)=>{
            if(onFinish && ev.target.parentNode == root)
                onFinish(ev.target);
        };
        
        root.addEventListener("dragstart", onStart, false);
        root.addEventListener("dragover", onDrag, false);
        root.addEventListener("dragend", onEnd, false);
        root.addEventListener("mouseover", onHover, false);
        root.addEventListener("mouseout", onUnhover, false);
        [].slice.call(root.children).forEach((el)=>{el.draggable = true;});
        return root;
    };

    self.init = ()=>{
        if(list.classList.contains("editable")){
            self.loadOrder();
            self.makeDraggable(items, (el)=>{
                self.makeImageActive(self.imageForSorter(el));
            }, (el)=>{
                self.sortChildren(images, (el)=>{
                    return self.idIndex(items, el.dataset.id);
                });
            }, (el)=>{
                self.imageForSorter(el).classList.remove("active");
                self.saveOrder();
            });

            self.makeDraggable(images, (el)=>{
                self.makeImageActive(el);
            }, (el)=>{
                self.sortChildren(items, (el)=>{
                    return self.idIndex(images, el.dataset.id);
                });
            }, (el)=>{
                el.classList.remove("active");
                self.saveOrder();
            });
        } else {
            items.addEventListener("mouseover", (ev)=>{
                if(ev.target.parentNode == items) {
                    self.makeImageActive(self.imageForSorter(ev.target));
                }
            }, false);
            items.addEventListener("mouseout", (ev)=>{
                if(ev.target.parentNode == items) {
                    self.imageForSorter(ev.target).classList.remove("active");
                }
            }, false);
            images.addEventListener("mouseover", (ev)=>{
                if(ev.target.parentNode == images) {
                    self.makeImageActive(ev.target);
                }
            }, false);
            images.addEventListener("mouseout", (ev)=>{
                if(ev.target.parentNode == images) {
                    ev.target.classList.remove("active");
                }
            }, false);
        }
    }

    self.init();
};
var toplists = new Toplists();
