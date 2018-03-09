var ToplistEditor = function(editor){
    var self = this;
    this.items = editor.querySelector(".items");
    console.log(self.items);
    self.init();
};

ToplistEditor.prototype.insertItem = function(prot){
    var self = this;
    var item = prot.cloneNode(true);
    var inputs = [].slice.call(item.querySelectorAll("input"));
    inputs.forEach((input)=>input.value = "");
    var first = inputs[0];
    var listener = ()=>{
        self.insertItem(item);
        first.removeEventListener("focus", listener);
    };
    first.addEventListener("focus", listener, false);
    self.items.appendChild(item);
};

ToplistEditor.prototype.init = function(){
    var self = this;
    var last = [].slice.call(self.items.querySelectorAll(".item")).pop();
    self.insertItem(last);
};

var Toplist = function(list){
    var self = this;
    self.list = list;
    self.id = list.dataset.id;
    self.images = list.querySelector(".images");
    self.items = list.querySelector(".items");
    self.init();
};

Toplist.prototype.idIndex = function(root, id){
    var i = 0;
    for(var el of root.children){
        if(el.dataset.id == id) return i;
        i++;
    }
    return -1;
};

Toplist.prototype.sortChildren = function(container, score){
    var items = [...container.children];
    
    items.sort((a, b) => score(a) - score(b))
        .forEach(item => container.appendChild(item));
    return container;
};

Toplist.prototype.saveOrder = function(){
    var order = [];
    for(var el of this.items.children){
        order.push(el.dataset.id);
    }
    window.localStorage.setItem("order-"+this.id, JSON.stringify(order));
    return order;
};

Toplist.prototype.loadOrder = function(){
    var order = JSON.parse(window.localStorage.getItem("order-"+this.id)||"[]");
    this.sortChildren(this.images, (el)=> order.indexOf(el.dataset.id));
    this.sortChildren(this.items, (el)=> order.indexOf(el.dataset.id));
    return this.images;
};

Toplist.prototype.imageForSorter = function(sorter){
    return this.images.children[this.idIndex(this.images, sorter.dataset.id)];
};

Toplist.prototype.makeImageActive = function(image){
    if(!image.classList.contains("active")){
        [].slice.call(this.images.children).forEach((img)=>img.classList.remove("active"));
        image.classList.add("active");
    }
    return image;
};

Toplist.prototype.makeDraggable = function(root, onBegin, onMove, onFinish){
    var self = this;
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

Toplist.prototype.init = function(){
    var self = this;
    if(self.list.classList.contains("editable")){
        self.loadOrder();
        self.makeDraggable(self.items, (el)=>{
            self.makeImageActive(self.imageForSorter(el));
        }, (el)=>{
            self.sortChildren(self.images, (el)=>{
                return self.idIndex(self.items, el.dataset.id);
            });
        }, (el)=>{
            self.imageForSorter(el).classList.remove("active");
            self.saveOrder();
        });

        self.makeDraggable(self.images, (el)=>{
            self.makeImageActive(el);
        }, (el)=>{
            self.sortChildren(self.items, (el)=>{
                return self.idIndex(self.images, el.dataset.id);
            });
        }, (el)=>{
            el.classList.remove("active");
            self.saveOrder();
        });
    } else {
        self.items.addEventListener("mouseover", (ev)=>{
            if(ev.target.parentNode == self.items) {
                self.makeImageActive(self.imageForSorter(ev.target));
            }
        }, false);
        self.items.addEventListener("mouseout", (ev)=>{
            if(ev.target.parentNode == self.items) {
                self.imageForSorter(ev.target).classList.remove("active");
            }
        }, false);
        self.images.addEventListener("mouseover", (ev)=>{
            if(ev.target.parentNode == self.images) {
                self.makeImageActive(ev.target);
            }
        }, false);
        self.images.addEventListener("mouseout", (ev)=>{
            if(ev.target.parentNode == self.images) {
                ev.target.classList.remove("active");
            }
        }, false);
    }
};

document.addEventListener("DOMContentLoaded", ()=>{
    var lists = document.querySelectorAll(".list");
    [].slice.call(lists).forEach((list)=>new Toplist(list));
    var editors = document.querySelectorAll(".editor");
    [].slice.call(editors).forEach((editor)=>new ToplistEditor(editor));
}, false);
