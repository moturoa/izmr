
var timeout = 0;

document.addEventListener("DOMContentLoaded", function () {
  const tableElem = document.querySelector(".result-table-container");
  const namespacedId = tableElem?.getAttribute("data-tableclickedid");
});



// Reset button is clicked on the search form
function resetform() {
     
     var a = $(".izmsearch");
     
     for (var i = 0; i < a.length; i++) {
        a[i].value = "";
     } 
      
    var el = $('#searchresults')
    
    if(el.children().length > 0){
      
       $('#searchresults').DataTable().clear().destroy();
       
       $('#searchresults_ui_wrapper').empty();
       $('#searchresults_ui_wrapper').append("<table id=\"searchresults\" class=\"display\"></table>");
       
       Shiny.setInputValue("izm_reset_form_clicked", Math.random()); 
      
    }
      
}
  

        
// Set window title to searched person
function loadPerson(id) {
         window.top.location = window.top.location + "/tabCasus/" + id;
}
       

// Throttled search function
function autosearch(searchurl, throttle, ns) {
  $('#' + ns + 'searchresults').display = "block";
  
  if(timeout) {
    clearTimeout(timeout);
    timeout = null;
  }
  timeout = setTimeout(autosearch2, throttle, searchurl, ns);
}


// Actual search function (used in throttled version above)
// Makes call to filldatatable()
function autosearch2(searchurl, ns) { 
  
        const container = document.querySelector('#' + ns + 'form_ui_wrapper.form-container');
        
        const form = container.querySelector(`form[name="${ns}izmsearch"]`);
        
        // Search boxes and their values on the IZM search page
        searchvalues = [
          form.elements[ns + "achternaam"].value,
          form.elements[ns + "bsn"].value,
          form.elements[ns + "postcode"].value,
          form.elements[ns + "straatnaam"].value,
          form.elements[ns + "huisnummer"].value,
          form.elements[ns + "geboortedatum"].value
        ];
                            
                for (i=0; i < form.length; i += 1) {
                     
                    // vervang wildcards
                    searchvalues[i] = searchvalues[i].split('%').join('[%]')
                    searchvalues[i] = searchvalues[i].split('_').join('[_]')
                
                    searchvalues[i] = searchvalues[i].split('***').join('%')
                    searchvalues[i] = searchvalues[i].split('*').join('_')   
                 
                    // birth date search
                    // !!! ASSUMED BIRTHDAY IS 6TH INPUT FIELD ABOVE!!!
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
               
                
                fill_datatable(searchvalues, searchurl, ns);
            
        }
        
        

// searchvalues: array with fixed order of search values (naam to geboortedatum below)
// 
function fill_datatable(searchvalues,searchurl,ns) {
  
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
                  
                  //console.log("Input to formatSearchResults")
                  //console.log(res)
                  
                  justDataTable(formatSearchResults(res, ns), ns); 
                },
                
                error:function(err){
                    console.log(err);
                }
                
      }))  }
            
             
}




            

setClickedId = function(id, shinyid){
  Shiny.setInputValue(shinyid, 
                        { 
                          id: id, 
                          nonce: Math.random()
                        });
  
}



                        
// This function formats the output from the search API
function formatSearchResults(data, ns) { 
    var len = data.recordsFiltered;

    Shiny.setInputValue("izm-izmnresults", len);
    
    const tableElem = document.querySelector('#' + ns + 'searchresults_ui_wrapper.result-table-container');
    const namespacedId = tableElem?.getAttribute("data-tableclickedid");
    
    // Uncomment these two lines to seelook at the data input  
    //console.log("input data to formatSearchResults");
    //console.log(data.data[0]);
      
    // Integer value of data.data[i][<<integer>>] refers to the position in the JSON 'dataframe'
    // returned by the search API. It is also used in the function below justDataTable(),
    // which selects columns to diplay in the datatable
    for (i = 0; i < len; i += 1) {  
    
        // Maak link naar casus    
        // make bsn clickable
        data.data[i][1] = "<a style='cursor: pointer;' href='javascript:;' onclick=\"setClickedId('" + 
                      data.data[i][0] + "', '" + 
                      namespacedId + "')\">" + 
                      data.data[i][1] + "</a>"
        
        // move datum overlijden to just after Geboortedatum (the 5th position was empty)
        // datum overlijden was the last column; is now in position 5 
        // first move to end
        data.data[i][12] = data.data[i][11]
        
        // move woonplaats to field 11 (after formatted address below)
        data.data[i][11] = data.data[i][10]
        
        // formatted adres in field 10
        data.data[i][10] = data.data[i][5] + ' ' + data.data[i][6] + ' ' +
                           data.data[i][7] + ' ' + data.data[i][8]
                           
        // now move datum overlijden again so that is is just after geboortedatum
        data.data[i][5] = data.data[i][12]
                           
        
    }
    
    //console.log("output data from formatSearchResults");
    //console.log(data.data[0]);
    
    return (data);
}
        
function justDataTable(payload, ns) {
  const $root = $('#' + ns + 'searchresults');          // wrapper
  let   $table = $root.find('table');
  if ($table.length && $.fn.dataTable.isDataTable($table)) {
    $table.DataTable().clear().destroy();
    // Remove the generated header/body to avoid ghost columns
    $table.remove();
  }
  // Fresh table element
  $root.empty().append('<table class="display" style="width:100%"></table>');
  $table = $root.find('table');
  return $table.DataTable({
    paging: true,
    pageLength: 25,
    dom: 'rtip',
    data: payload.data,
    columnDefs: [
      { targets: 0,  visible: false },
      { title: 'BSN',          targets: 1 },
      { title: 'Naam',         targets: 2 },
      { title: 'Voornamen',    targets: 3 },
      { title: 'Geboortedatum',targets: 4, render: DataTable.render.date() },
      { title: 'Ovl. datum',   targets: 5, render: DataTable.render.date() },
      { targets: 6,  visible: false },
      { targets: 7,  visible: false },
      { targets: 8,  visible: false },
      { title: 'Postcode',     targets: 9 },
      { title: 'Adres',        targets: 10 },
      { title: 'Woonplaats',   targets: 11 }
    ]
  });
}       
        
function xxxxjustDataTable(data, ns) {
   
   var table = $('#' + ns + 'searchresults');
   
   $('#' + ns + 'searchresults').DataTable({ 
        "processing": false,
        "serverSide": false,
        destroy: true,
        paging: true,
        pageLength: 25,
        dom: 'rtip',
        
        data:data.data,
            columnDefs: [

              // titles and display options for the columns out of formatSearchResults
              // Note that we have to keep the order: if you want to move the order of columns,
              // do that in formatSearchResults(), because here we have to config in increasing order
              // for some reason
              // 'targets' refers to the column position in the data out of formatSearchResults (0 = 1st column)
              { "targets" : 0, "visible" : false },
              { "title": "BSN", "targets" : 1 },
              { "title": "Naam", "targets" : 2 },
              { "title": "Voornamen",  "targets" : 3 },
              { "title": "Geboortedatum",  
                "targets" : 4,
                "render": DataTable.render.date()
              },
              { "title" : "Ovl. datum", 
                "targets" : 5,  
                "render": DataTable.render.date()
              },
              { "targets" : 6, "visible" : false },
              { "targets" : 7, "visible" : false },
              { "targets" : 8, "visible" : false },
              { "title": "Postcode",  "targets" : 9 },
              { "title": "Adres", "targets" : 10 },
              { "targets" : 11, "title" : "Woonplaats"}

            ],
})
  
}
        