
var timeout = 0;

function resetform() {
     
     var a = $(".izmsearch");
     
     for (var i = 0; i < a.length; i++) {
        a[i].value = "";
     } 
      
     $('#searchresults').DataTable().clear().destroy();
     
     $('#searchresults_ui_wrapper').empty();
     $('#searchresults_ui_wrapper').append("<table id=\"searchresults\" class=\"display\"></table>");
     
     Shiny.setInputValue("izm_reset_form_clicked", Math.random());

}
        

        
        
function loadPerson(id) {
         window.top.location = window.top.location + "/tabCasus/" + id;
}
       

        
function autosearch(searchurl, throttle) {
  
  $('#searchresults').display = "block";
  
  if(timeout) {
    clearTimeout(timeout);
    timeout = null;
  }
  timeout = setTimeout(autosearch2, throttle, searchurl);
}



function autosearch2(searchurl) { 
        
        
        let form = document.forms.izmsearch;
        
        searchvalues = [
          form.elements.achternaam.value,
          form.elements.bsn.value,
          form.elements.postcode.value,
          form.elements.straatnaam.value,
          form.elements.huisnummer.value,
          form.elements.geboortedatum.value
        ];
        
        //alert(form.elements.postcode.value);
                            
                for (i=0; i < form.length; i += 1) {
                     
                    // vervang wildcards
                    searchvalues[i] = searchvalues[i].split('%').join('[%]')
                    searchvalues[i] = searchvalues[i].split('_').join('[_]')
                
                    searchvalues[i] = searchvalues[i].split('***').join('%')
                    searchvalues[i] = searchvalues[i].split('*').join('_')   
                 
                    // birth date search
                    if (i == 5) {    
                      
                      // remove "-" in search term
                      searchvalues[i]= searchvalues[i].split('-').join('')  
                      
                      // search starts with wildcard   
                      if(searchvalues[i][0] == '%' ){
                        if (searchvalues[i].length <= 5) searchvalues[i] = searchvalues[i].slice(1,searchvalues[i].length);
                        if (searchvalues[i].length == 6)searchvalues[i] = searchvalues[i][2] +  searchvalues[i][3] +  searchvalues[i][4] +  searchvalues[i][5] +  '_' + searchvalues[i][1];
                        if (searchvalues[i].length == 7) searchvalues[i] = searchvalues[i][3] +  searchvalues[i][4] +  searchvalues[i][5] +  searchvalues[i][6] + searchvalues[i][1] + searchvalues[i][2];
                        if (searchvalues[i].length == 8) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] +  searchvalues[i][6] +  searchvalues[i][7] + searchvalues[i][2] + searchvalues[i][3] + '_' + searchvalues[i][2];
                      }
                      
                      else if(searchvalues[i].length == 8) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] + searchvalues[i][6] + searchvalues[i][7] + searchvalues[i][2] + 
                        searchvalues[i][3] + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 7) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] + searchvalues[i][6] + '_' + searchvalues[i][2] +  
                        searchvalues[i][3] + searchvalues[i][0] +  searchvalues[i][1];
                        
                      else if(searchvalues[i].length == 6) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] + '_'+ '_' + searchvalues[i][2] +  
                        searchvalues[i][3] + searchvalues[i][0] +  searchvalues[i][1];
                        
                      else if(searchvalues[i].length == 5) searchvalues[i] = searchvalues[i][4] + '_' + '_' + '_' + searchvalues[i][2] +  
                        searchvalues[i][3] + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 4) searchvalues[i] = '_' + '_' + '_' + '_' + searchvalues[i][2] +  
                        searchvalues[i][3] + searchvalues[i][0] +  searchvalues[i][1];
                        
                      else if(searchvalues[i].length == 3) searchvalues[i] = '_' + '_' + '_' + '_' + 
                        searchvalues[i][2] +  '_' + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 2) searchvalues[i] = '_' + '_' + '_'  +'_' + '_' +  '_' + 
                        searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 1) searchvalues[i] = '_' + '_' + '_' + '_' + '_' +  '_' + searchvalues[i][0] + '_'; 
                    }
                     
                    // autosearch functionaliteit: add '%' at the end   
                    if(searchvalues[i].substring(searchvalues[i].length-1) != '%' && 
                       i != 4 && searchvalues[i].length > 0){
                        searchvalues[i] = searchvalues[i].concat('%') 
                    }  
                    
                    // autosearch namen: aff '%' at beginning
                    if (i == 0 && searchvalues[i].substring(0,1) != '%') {    
                      searchvalues[i] =  '%'.concat(searchvalues[i]) 
                    }
                    
                } 
              
        
        //alert(searchvalues);
               
                
                fill_datatable(searchvalues, searchurl);
            
        }
        
        
function fill_datatable(searchvalues,searchurl) {
  
  
 
      //alert(zoeken_achternaam);
            
      if(searchvalues){
        
        return($.ajax( { 
                url: searchurl,  
                // cannot use contentType because it invokes HTTP400
                //, "contentType": "application/json; charset=utf-8"  
                dataType: "json",
                type: "POST", 
                data: {     
                    rest_version: false, 
                    naam: searchvalues[0],
                    from_id: searchvalues[1],
                    postcode: searchvalues[2],
                    straatnaam: searchvalues[3],
                    huisnummer: searchvalues[4],
                    geboortedatum: searchvalues[5]
                },
                
                // cannot use success https://datatables.net/forums/discussion/42156/server-side-processing-stuck-on-processing-when-handing-success-in-ajax
                
                success: function (res) { 
                    justDataTable(formatSearchResults(res)); 
                },
                
                error:function(err){
                    console.log(err);
                }
                
      }))  }
            
             
}

function justDataTable(data) {
   
   //console.log(data);
   
   $('#searchresults').DataTable({ 
     "processing": false,
        "serverSide": false,
        destroy: true,
        paging: true,
        pageLength: 25,
        dom: 'rtip',
        
        data:data.data,
            columnDefs: [

              { "targets" : 0, "visible" : false },
              { "title": "BSN", "targets" : 1 },
              { "title": "Naam", "targets" : 2 },
              { "title": "Voornamen",  "targets" : 3 },
              { "title": "Geboortedatum",  
                "targets" : 4,
                "render": DataTable.render.date()
              },
              { "title": "Adres", "targets" : 10 },
              { "title": "Postcode",  "targets" : 9 },
              { "targets" : 5, "visible" : false },
              { "targets" : 6, "visible" : false },
              { "targets" : 7, "visible" : false },
              { "targets" : 8, "visible" : false }
            ],
})
  
}


            

setClickedId = function(id, shinyid){
  Shiny.setInputValue(shinyid, 
                        { 
                          id: id, 
                          nonce: Math.random()
                        });
  
}



                        

function formatSearchResults(data) { 
  
            var len = data.recordsFiltered;
 
            Shiny.setInputValue("izm-izmnresults", len);
              
            //console.log("input data to formatSearchResults");
            //console.log(data.data[0]);
              
            // Maak link naar casus
            for (i = 0; i < len; i += 1) {  
                
                // make bsn clickable
                data.data[i][1] = "<a style=\"cursor: pointer;\" onclick=\"setClickedId('" + 
                                     data.data[i][0] + "', 'izm-izmclickedid')\">" + 
                                     data.data[i][1] + "</a>" 
                
                // formatted adres in field 10
                data.data[i][10] = data.data[i][5] + ' ' + data.data[i][6] + ' ' +
                                   data.data[i][7] + ' ' + data.data[i][8]
                
            }
            
            //console.log("output data from formatSearchResults");
            //console.log(data.data[0]);
            
            return (data);
        }
        
        
        