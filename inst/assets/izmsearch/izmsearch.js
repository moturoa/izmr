
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
                 
                    if (i == 5) {    
                      searchvalues[i]= searchvalues[i].split('-').join('')  
                         
                      if(searchvalues[i][0] == '%' ){
                        if (searchvalues[i].length <= 5) searchvalues[i] = searchvalues[i].slice(1,searchvalues[i].length);
                        if (searchvalues[i].length == 6)searchvalues[i] = searchvalues[i][2] +  searchvalues[i][3] +  searchvalues[i][4] +  searchvalues[i][5] +  '-_' + searchvalues[i][1];
                        if (searchvalues[i].length == 7) searchvalues[i] = searchvalues[i][3] +  searchvalues[i][4] +  searchvalues[i][5] +  searchvalues[i][6] + '-' +  searchvalues[i][1] + searchvalues[i][2];
                        if (searchvalues[i].length == 8) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] +  searchvalues[i][6] +  searchvalues[i][7] + '-' + searchvalues[i][2] + searchvalues[i][3] + '-_' + searchvalues[i][2];
                      }
                      else if(searchvalues[i].length == 8) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] + searchvalues[i][6] + searchvalues[i][7] + '-' + searchvalues[i][2] +  searchvalues[i][3] + '-' + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 7) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] + searchvalues[i][6] + '_-' + searchvalues[i][2] +  searchvalues[i][3] + '-' + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 6) searchvalues[i] = searchvalues[i][4] +  searchvalues[i][5] + '_'+ '_-' + searchvalues[i][2] +  searchvalues[i][3] + '-' + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 5) searchvalues[i] = searchvalues[i][4] + '_' + '_' + '_-' + searchvalues[i][2] +  searchvalues[i][3] + '-' + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 4) searchvalues[i] = '_' + '_' + '_' + '_-' + searchvalues[i][2] +  searchvalues[i][3] + '-' +  searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 3) searchvalues[i] = '_' + '_' + '_' + '_-' + searchvalues[i][2] +  '_-' + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 2) searchvalues[i] = '_' + '_' + '_'  +'_-' + '_' +  '_-' + searchvalues[i][0] +  searchvalues[i][1];
                      else if(searchvalues[i].length == 1) searchvalues[i] = '_' + '_' + '_' + '_-' + '_' +  '_-' + searchvalues[i][0] + '_'; 
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
              
                $('#searchresults').DataTable().destroy();
                
                fill_datatable(searchvalues, searchurl);
            
        }
        
        
function fill_datatable(searchvalues,searchurl) {
  
  
 
      //alert(zoeken_achternaam);
            
      if(searchvalues){
        var dataTable = $('#searchresults').DataTable({
            "processing": true,
            "serverSide": true,
            "ordering": false, 
            "searching": false,
            "pageLength": 15,
            "dom": 'rtip',
            "columnDefs": [
              { "title": "BSN", "targets" : 0 },
              { "title": "Naam", "targets" : 1 },
              { "title": "Geboortedatum", "targets" : 2 },
              { "title": "Straat", "targets" : 3 },
              { "title": "Huisnummer", "targets" : 4 },
              { "title": "Huisletter", "targets" : 5 },
              { "title": "Postcode", "targets" : 6 }
            ],
            "ajax": { 
                url: searchurl,    
                dataType: "json",
                type: "POST", 
                data: {     
                    rest_version: true, 
                    naam: searchvalues[0],
                    from_id: searchvalues[1],
                    postcode: searchvalues[2],
                    straatnaam: searchvalues[3],
                    huisnummer: searchvalues[4],
                    geboortedatum: searchvalues[5]
                },
                // cannnot use success https://datatables.net/forums/discussion/42156/server-side-processing-stuck-on-processing-when-handing-success-in-ajax
                dataFilter: function(reps) { 
                    return formatSearchResults(reps);
                  },
                  error:function(err){
                        console.log(err);
                  }
                
                
                
             
      } 
        }); 
        console.log(dataTable)
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
 
            var len = data.recordsFiltered;
 
            Shiny.setInputValue("izm-izmnresults", len);
            
            // Maak link naar casus
            for (i = 0; i < len; i += 1) {  
                
                // make bsn clickable
                data[i][1] = "<a style=\"cursor: pointer;\" onclick=\"setClickedId('" + 
                                     data[i][0] + "', 'izm-izmclickedid')\">" + 
                                     data[i][1] + "</a>" 
                
                // format birthdate
                date = data[i][3]
                data[i][3] = date.slice(8,10)+ '-' + date.slice(5,7) + '-' + date.slice(0,4) 
                
                // abbreviate
                data[i] = data[i].slice(1,8);
            }
            console.log(data)
            return (data);
        }
        
        
        