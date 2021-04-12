
var timeout = 0;

function resetform() {
     
     var a = $(".izmsearch");
     
     for (var i = 0; i < a.length; i++) {
        a[i].value = "";
     } 
      
     $('#searchresults').DataTable().clear().destroy();
     
}
        

        
        
function loadPerson(id) {
         window.top.location = window.top.location + "/tabCasus/" + id;
}
       

        
function autosearch(searchurl, throttle) {
  
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
              
                $('#searchresults').DataTable().destroy();
                
                fill_datatable(searchvalues, searchurl);
            
        }
        
        
function fill_datatable(searchvalues,searchurl) {
            
      //alert(zoeken_achternaam);
            
      if(searchvalues){
        var dataTable = $('#searchresults').DataTable({
            "processing": true,
            "serverSide": true,
            "order": [], 
            "searching": false,
            "pageLength": 15,
            "dom": 'rtip',
            "ajax": { 
                url: searchurl,  
                jsonpCallback: "formatSearchResults",
                dataType: "jsonp", 
                data: {   
                    rest_version: true, 
                    naam: searchvalues[0],
                    from_id: searchvalues[1],
                    postcode: searchvalues[2],
                    straatnaam: searchvalues[3],
                    huisnummer: searchvalues[4],
                    geboortedatum: searchvalues[5]
                }
            }
        }); 
         
          
        return(dataTable) 
        
      }  
            
             
}



setClickedId = function(id, shinyid){
  Shiny.setInputValue(shinyid, 
                        { 
                          id: id, 
                          nonce: Math.random()
                        });
  
}



                        

function formatSearchResults(data) {
  
            var len = data.data.length;
            
            Shiny.setInputValue("izm-izmnresults", len);
            
            // Maak link naar casus
            for (i = 0; i < len; i += 1) {  
                
                // make bsn clickable
                data.data[i][1] = "<a style=\"cursor: pointer;\" onclick=\"setClickedId('" + 
                                     data.data[i][0] + "', 'izm-izmclickedid')\">" + 
                                     data.data[i][1] + "</a>" 
                
                // format birthdate
                date = data.data[i][3]
                data.data[i][3] = date.slice(8,10)+ '-' + date.slice(5,7) + '-' + date.slice(0,4) 
                
                // abbreviate
                data.data[i] = data.data[i].slice(1,8);
            }
            
            return (data);
        }
        
        
        