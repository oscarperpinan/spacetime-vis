highlight = function(evt){
    evt.target.setAttribute('opacity', '1');
    evt.target.setAttribute('stroke', 'red');
    evt.target.setAttribute('stroke-width', '1');
}

hide = function(evt){
    evt.target.setAttribute('opacity', '0.3');
    evt.target.setAttribute('stroke', 'black');
    evt.target.setAttribute('stroke-width', '0.3');
}
