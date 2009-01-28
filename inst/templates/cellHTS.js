toggleSelectionState = function(id, thisTable)
{
    var tables = document.getElementsByTagName("table");
    var tl = tables.length;
    var cl;
    var match;
    for(var i=0; i<tl; i++)
    {
        cl = tables[[i]].className;
	mt = cl.match(id);
        if(mt)
	    tables[[i]].className = cl.replace(/ selected/, " unselected");
    }
    thisTable.className = thisTable.className.replace(/ unselected/, " selected");
}


toggleImageVisibility = function(id)
    {
	var tables = document.getElementsByTagName("table");
	var tl = tables.length;
	var cl;
	var mtid; var mtChanRep;
	for(var i=0; i<tl; i++)
	{
            cl = tables[[i]].className;
	    mtId = cl.match(id);
	    mtChanRep = cl.match("channel"+currentChannel+" replicate"+currentReplicate);
	    if(mtId)
		tables[[i]].className = cl.replace(/ visible/, " invisible");
	    if(mtChanRep)
		tables[[i]].className = cl.replace(/ invisible/, " visible");
	}
    }


function toggleTabById(id, thisTable, src)
{
    toggleSelectionState(id, thisTable);   
    document.getElementsByTagName("iframe")[0].src=src; 
}

    var currentReplicate=1;
    var currentChannel=1;



function toggleTabByChannel(id, thisTable, channel)
{
    toggleSelectionState(id, thisTable);  
    currentChannel = channel;
    id = id.replace(/Channel/, "");
    toggleImageVisibility(id);
}


    function toggleTabByReplicate(id, thisTable, replicate)
{
    toggleSelectionState(id, thisTable);  
    currentReplicate = replicate;
    id = id.replace(/Replicate/, "");
    toggleImageVisibility(id);
}



function linkToPdf(url)
{
    document.location.href = url;
}


function linkToFile(url)
{
     document.location.href = url; 
}




function adjustIFrameSize (iframeWindow) 
{
    if (iframeWindow.document.height) {
	var iframeElement = document.getElementById
	(iframeWindow.name);
	iframeElement.style.height = iframeWindow.document.height + 25 + 'px';
	iframeElement.style.width = iframeWindow.document.width  + 'px';
    }
    else if (document.all) {
	var iframeElement = document.all[iframeWindow.name];
	if (iframeWindow.document.compatMode &&
            iframeWindow.document.compatMode != 'BackCompat') 
	{
	    iframeElement.style.height = 
		iframeWindow.document.documentElement.scrollHeight + 25 + 'px';
	    iframeElement.style.width = 
		iframeWindow.document.documentElement.scrollWidth + 5 + 'px';
	}
	else {
	    iframeElement.style.height = 
		iframeWindow.document.body.scrollHeight + 25 + 'px';
	    iframeElement.style.width = 
		iframeWindow.document.body.scrollWidth + 5 + 'px';
	}
    }
}