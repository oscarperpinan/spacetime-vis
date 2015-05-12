$(document).ready(function(){
    $(".figure a").colorbox({height:"100%"});
    $(".svg").colorbox({
	iframe:true, 
	height:"125%", 
	width:"70%",
	scrolling:false
    });
    $(".vimeo").colorbox({
	iframe:true, 
	innerWidth:600, 
	innerHeight:600, 
	scrolling:false
    });
});
