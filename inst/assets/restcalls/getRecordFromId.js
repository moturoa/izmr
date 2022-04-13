
Shiny.addCustomMessageHandler("getRecordFromId", function(params) {    
  console.log(params)
  
 if (params.pseudo_ids == null) {   
    Shiny.setInputValue(params.id, Math.random())
 } else if (params.pseudo_ids.length <= 0) {   
    Shiny.setInputValue(params.id, Math.random())
 } else {  
 
  $.ajax({ 
      url: params.rest_url,
      dataType: "json",
          type: "get", 
      data: {   
          pseudo_ids: params.pseudo_ids
      },
      success: function (jsonResults) {  
        
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
        dataType: "json",
            type: "get", 
        data: {   
            postcode: params.postcode,
            huisnummer: params.huisnummer,
            huisletter: params.huisletter
        },
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







