
Shiny.addCustomMessageHandler("getRecordFromId", function(params) {    
  
  
  console.log("params.id (getRecordFromId)")
  console.log(params.pseudo_ids)
  
 if (params.pseudo_ids == null) {   
    Shiny.setInputValue(params.id, Math.random())
 } else if (params.pseudo_ids.length <= 0) {   
    Shiny.setInputValue(params.id, Math.random())
 } else {  
 
  $.ajax({ 
      url: params.rest_url,
      contentType: "application/json; charset=utf-8",
      dataType: "json",
      type: "POST", 
      data: JSON.stringify({   
          pseudo_ids: params.pseudo_ids
      }),
      success: function (jsonResults) {  
        
        
        //console.log(jsonResults.data);
        
        // NOTE: calling param.id will add namespace around label
         Shiny.setInputValue(params.id, jsonResults.data)
         
      }, error: function(data) {             
         console.log("Error");    
         console.log(data);
         Shiny.setInputValue(params.id, data);
      }
  });
  
 }    
});

Shiny.addCustomMessageHandler("getRecordFromAdress",  function(params) {    
 
 if (params.postcode == null) {  
    Shiny.setInputValue(params.id, Math.random())
 } else{
    $.ajax({ 
        url: params.rest_url,
        contentType: "application/json; charset=utf-8",
      dataType: "json",
        type: "POST", 
        data:  JSON.stringify({   
            postcode: params.postcode,
            huisnummer: params.huisnummer,
            huisletter: params.huisletter
        }),
        success: function (jsonResults) {  
         
          console.log(jsonResults.data)
          // NOTE: calling param.id will add namespace around label
           Shiny.setInputValue(params.id, jsonResults.data)
           
        } , error: function(data) {             
         console.log("Error");    
         console.log(data);
         Shiny.setInputValue(params.id, data);
      }
    });
 }
});

Shiny.addCustomMessageHandler("setInput",  function(params) {    
  Shiny.setInputValue(params.pid, params.data)
});







