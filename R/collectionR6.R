#' R6 object for collections of items
#' @description Keep a list of items, methods to remove, add, remove/add based on a checkbox-type input,
#' history, etc.
#' @export
Collection <- R6::R6Class("Collection", 
                      
                      public = list(
                        key_name = NULL,
                        
                        initialize = function(key_name){
                          
                          if(!is.character(key_name)){
                            stop("Must give quoted variable name as key.")
                          }
                          self$key_name <- key_name
                          
                        },
                        
                        history = data.frame(),
                        
                        members = c(),
                        members_rv = reactiveValues(id = c(), nonce = runif(1)),
                        
                        add = function(id, duplicate = FALSE){
                          for(i in id){
                            if(!self$is_member(i) | duplicate){
                              self$members <- c(self$members, i)
                              self$members_rv$id <- c(self$members_rv$id, i)
                              private$bind_history(id = i, action = "add")
                            }
                            self$members_rv$nonce <- runif(1)
                          }
                        },
                        delete = function(id){
                          for(i in id){
                            if(i %in% self$members){
                              self$members <- setdiff(self$members, i)
                              self$members_rv$id <- setdiff(self$members_rv$id, i)
                              private$bind_history(id = i, action = "delete")
                            }
                          }
                        },
                        # Add a member if chk==TRUE, otherwise remove it (if possible)
                        checkbox_adjust = function(id, chk, toast = FALSE){
                          
                          
                          if(chk){
                            
                            if(!self$is_member(id)){
                              self$add(id)
                              
                              if(toast){
                                toastr_success(glue("Adres toegevoegd aan verzameling."))
                              }
                            }
                            
                          } else {
                            
                            if(self$is_member(id)){
                              self$delete(id)
                              
                              if(toast){
                                toastr_success(glue("Adres verwijderd uit verzameling."))
                              }
                            }
                          }
                          
                        },
                        
                        add_or_delete = function(id){
                          if(self$is_member(id)){
                            self$delete(id)
                          } else {
                            self$add(id)
                          }
                        },
                        
                        clear = function(){
                          for(id in self$members){
                            self$delete(id)
                          }
                        },
                        is_member = function(id){
                          id %in% self$members
                        },
                        members_df = function(){
                          
                          tibble(!!sym(self$key_name) := self$members)
                          
                        },
                        n = function(){
                          length(self$members)
                        }
                        
                        
                        
                      ),
                      private = list(
                        
                        bind_history = function(id = NA, action = NA){
                          self$history <- rbind(self$history, data.frame(timestamp = format(Sys.time()),
                                                                         id = id,
                                                                         action = action
                          ))
                        }
                        
                      ))
