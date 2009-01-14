function selectTab(id,off){
    var rep;
    if(off) rep = "unselected"; else rep = "selected";
    var obj = document.getElementById(id+"_1");
    obj.className = "tab left "+rep;
    obj = document.getElementById(id+"_2");
    obj.className = "tab middle "+rep;    
    obj = document.getElementById(id+"_3");
    obj.className = "tab "+rep;
    obj = document.getElementById(id+"_4");
    obj.className = "tab right "+rep;
}

    function toggleTabs(id, total, src){
	for(var j=1; j<=total; j++){
	    selectTab("tab"+j,true);
	}
	selectTab(id,false);
	document.getElementsByTagName("iframe")[0].src=src;
    }