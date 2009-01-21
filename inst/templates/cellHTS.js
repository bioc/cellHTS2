function selectTab(id,off)
{
    var rep;
    if(off) rep = "unselected"; else rep = "selected";
    var classes = new Array("tab left ", "tab middle ", "tab ", "tab right ");
    for(var j=1; j<=4; j++)
	document.getElementById("tab"+id+"_"+j).className = classes[j-1]+rep;
}
  
  
function toggleTabs(id, total, src)
{
    for(var j=1; j<=total; j++){
	selectTab(j,true);
    }
    selectTab(id,false);
    document.getElementsByTagName("iframe")[0].src=src;
}


function toggleImages(id, total)
{
    var classes = new Array("image header ", "image caption ", "image ", "image pdf ");
    for(var i=1; i<=total; i++){
	selectTab(i,true);
	for(var j=1; j<=3; j++)
	    document.getElementById("img"+i+"_"+j).className = classes[j-1]+"invisible";
    }
    selectTab(id,false);
    for(var j=1; j<=3; j++)
	document.getElementById("img"+id+"_"+j).className = classes[j-1];
}


function linkToPdf(url)
{
    document.location.href = url;
}



function adjustIFrameSize (iframeWindow) 
{
    if (iframeWindow.document.height) {
	var iframeElement = document.getElementById
	(iframeWindow.name);
	iframeElement.style.height = iframeWindow.document.height + 'px';
	iframeElement.style.width = iframeWindow.document.width + 'px';
    }
    else if (document.all) {
	var iframeElement = document.all[iframeWindow.name];
	if (iframeWindow.document.compatMode &&
            iframeWindow.document.compatMode != 'BackCompat') 
	{
	    iframeElement.style.height = 
		iframeWindow.document.documentElement.scrollHeight + 5 + 'px';
	    iframeElement.style.width = 
		iframeWindow.document.documentElement.scrollWidth + 5 + 'px';
	}
	else {
	    iframeElement.style.height = 
		iframeWindow.document.body.scrollHeight + 5 + 'px';
	    iframeElement.style.width = 
		iframeWindow.document.body.scrollWidth + 5 + 'px';
	}
    }
}