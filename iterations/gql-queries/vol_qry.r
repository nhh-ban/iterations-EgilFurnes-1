vol_qry <- 
  function(id, from, to){
    
    raw_qry <- 
      '
        {
          trafficData(trafficRegistrationPointId: ".start id .end") {
            volume {
              byHour(from: ".start from .end", to: ".start to .end") {
                edges {
                  node {
                    from
                    to
                    total {
                      volumeNumbers {
                        volume
                      }
                    }
                  }
                }
              }
            }
          }
        }
      '
    return(
      glue::glue(
        raw_qry, 
        .open=".start", 
        .close=".end"
      )
    )
  }