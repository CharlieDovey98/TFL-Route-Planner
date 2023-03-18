#lang racket
(require racket/gui)  ; import Rvket Gui library
(require graph)       ; import graph library
(require racket/trace); import trace functionality
(require racket/gui map-widget) ;import map-widget library
(require racket/date) ; import racket date and time capabilities
(require json)        ; import JSON file capabilities

                          #|      Group 4 TFL TRAVEL APP WITH GUI      Prototype 03 Graphs     |#
;              vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv Notes vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

; To Do:
; write to and read from a json file
; working map with plottable routes
; zoomable images of underground lines
; colourize syntax according to the line it belongs to
; add a "change to 'line'" when user needs to change lines during their route

;  (send (send this get-parent) saveRoute start destination route timeTaken lines "save.json")

;              vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv Defintions vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
; definitions for station names to prevent syntax errors when using them in code
; allows the ability to change a stations name once that will affect all instances of the station where used in the code saving time

; function allowing multiple definitions to listed in one go
(define-syntax-rule (definemore (a b) ...)
  (match-define (list a ...) (list b ...)))

; Map locations:
(define london #(51.5074 -0.1278))

(define image_folder (path->string (build-path (current-directory) "TFL-Route-Planner/Images"))) ;A path to the Image folder where all images are stored for the project
(define get_file_path (lambda (filename) (string-append image_folder "/" filename))) ; Gets a path for a specific file

; definitions for image locations
(definemore (Tube_map_image "Tube map.png")
  (Central_line_map "Central line.png")
  (Circle_line_map "Circle line.png")
  (District_line_map "District line.png")
  (Hammersmith_city_line_map "Hammersmith city line.png")
  (Jubilee_line_map "Jubilee line.png")
  (Metropolitan_line_map "Metropolitan line.png")
  (Northern_line_map "Northern line.png")
  (Piccadilly_line_map "Piccadilly line.png")
  (Victoria_line_map "Victoria line.png")
  (Waterloo_city_line_map "Waterloo city line.png")
  (Bakerloo_line_map "Bakerloo line.png"))

(define line_maps (list Tube_map_image Central_line_map Circle_line_map District_line_map Hammersmith_city_line_map Jubilee_line_map Metropolitan_line_map Northern_line_map
                        Piccadilly_line_map Victoria_line_map Waterloo_city_line_map Bakerloo_line_map))
                                     
; definitions for station names
(definemore (Walthamstow_Central "Walthamstow Central") (Blackhorse_Road "Blackhorse Road") (Tottenham_Hale "Tottenham Hale") (Seven_Sisters "Seven Sisters") (Finsbury_Park "Finsbury Park")
  (Highbury_&_Islington "Highbury & Islington") (Kings_Cross_St_Pancras "Kings Cross St Pancras") (Euston "Euston") (Warren_Street "Warren Street") (Oxford_Circus "Oxford Circus")
  (Green_Park "Green Park") (Victoria "Victoria") (Pimlico "Pimlico") (Vauxhall "Vauxhall") (Stockwell "Stockwell") (Brixton "Brixton")
  ;  Victoria Line Stations ^^^^              Circle Line Stations vvvvv
  (Hammersmith "Hammersmith") (Goldhawk_Road "Goldhawk Road") (Shepherd_s_Bush_Market "Shepherd's Bush Market") (Wood_Lane "Wood Lane") (Latimer_Road "Latimer Road")
  (Ladbroke_Grove "Ladbroke Grove") (Westbourne_Park "Westbourne Park") (Royal_Oak "Royal Oak") (Paddington "Paddington") (Edgware_Road "Edgware Road") (Baker_Street "Baker Street")
  (Great_Portland_Street "Great Portland Street") (Euston_Square "Euston Square") (Farringdon "Farringdon") (Barbican "Barbican") (Moorgate "Moorgate")
  (Liverpool_Street "Liverpool Street") (Aldgate "Aldgate") (Tower_Hill "Tower Hill") (Monument "Monument") (Cannon_Street "Cannon Street") (Mansion_House "Mansion House")
  (Blackfriars "Blackfriars") (Temple "Temple") (Embankment "Embankment") (Westminster "Westminster") (St_James_s_Park "St James's Park") (Sloane_Square "Sloane Square")
  (South_Kensington "South Kensington") (Gloucester_Road "Gloucester Road") (High_Street_Kensington "High Street Kensington") (Notting_Hill_Gate "Notting Hill Gate")
  (Bayswater "Bayswater")
  ;  Circle Line Stations ^^^^              Piccadilly Line Stations vvvvv
  (Cockfosters "Cockfosters") (Oakwood "Oakwood") (Southgate "Southgate") (Arnos_Grove "Arnos Grove") (Bounds_Green "Bounds Green") (Wood_Green "Wood Green") (Turnpike_Lane "Turnpike Lane")
  (Manor_House "Manor House") (Arsenal "Arsenal") (Holloway_Road "Holloway Road") (Caledonian_Road "Caledonian Road") (Russell_Square "Russell Square") (Holborn "Holborn")
  (Covent_Garden "Covent Garden") (Leicester_Square "Leicester Square") (Piccadilly_Circus "Piccadilly Circus") (Hyde_Park_Corner "Hyde Park Corner") (Knightsbridge "Knightsbridge")
  (Earl_s_Court "Earl's Court") (Barons_Court "Barons Court") (Turnham_Green "Turnham Green") (Acton_Town "Acton Town") (Ealing_Common "Ealing Common") (North_Ealing "North Ealing")
  (Park_Royal "Park Royal") (Alperton "Alperton") (Sudbury_Town "Sudbury Town") (Sudbury_Hill "Sudbury Hill") (South_Harrow "South Harrow") (Rayners_Lane "Rayners Lane")
  (Eastcote "Eastcote") (Ruislip_Manor "Ruislip Manor") (Ruislip "Ruislip") (Ickenham "Ickenham") (Hillingdon "Hillingdon") (Uxbridge "Uxbridge") (South_Ealing "South Ealing")
  (Northfields "Northfields") (Boston_Manor "Boston Manor") (Osterley "Osterley") (Hounslow_East "Hounslow East") (Hounslow_Central "Hounslow Central") (Hounslow_West "Hounslow West")
  (Hatton_Cross "Hatton Cross") (Heathrow_Terminals_1_2_and_3 "Heathrow Terminals 1, 2 and 3") (Heathrow_Terminal_4 "Heathrow Terminal 4") (Heathrow_Terminal_5 "Heathrow Terminal 5") 
  ;  Piccadilly Line Stations ^^^^              District Line Stations vvvvv
  (Upminster "Upminster") (Upminster_Bridge "Upminster Bridge") (Hornchurch "Hornchurch") (Elm_Park "Elm Park") (Dagenham_East "Dagenham East")(Dagenham_Heathway "Dagenham Heathway")
  (Becontree "Becontree") (Upney "Upney") (Barking "Barking") (East_Ham "East Ham") (Upton_Park "Upton Park") (Plaistow "Plaistow") (West_Ham "West Ham") (Bromley-by-Bow "Bromley-by-Bow")
  (Bow_Road "Bow Road") (Mile_End "Mile End") (West_Kensington "West Kensington")
  (Stepney_Green "Stepney Green") (Whitechapel "Whitechapel") (Aldgate_East "Aldgate East") (Tower_Hill "Tower Hill") (Cannon_Street "Cannon Street") (Mansion_House "Mansion House")
  (Edgware_Road "Edgware Road") (West_Brompton "West Brompton") (Kensington_Olympia "Kensington (Olympia)") (Fulham_Broadway "Fulham Broadway") (Parsons_Green "Parsons Green")
  (Putney_Bridge "Putney Bridge") (East_Putney "East Putney") (Southfields "Southfields") (Wimbledon_Park "Winbledon Park") (Wimbledon "Wimbledon") (Barons_Court "Barons Court")
  (Ravenscourt_Park "Ravenscourt Park") (Stamford_Brook "Stamford Brook") (Turnham_Green "Turnham Green") (Chiswick_Park "Chiswick Park") (Gunnersbury "Gunnersbury") (Kew_Gardens "Key Gardens")
  (Richmond "Richmond") (Ealing_Broadway "Ealing Broadway")
  ;  District Line Stations ^^^^              Central Line Stations vvvvv
  (West_Ruislip "West Ruislip") (Ruislip_Gardens "Ruislip Gardens") (South_Ruislip "South Ruislip") (Northolt "Northolt") (Greenford "Greenford") (Perivale "Perivale")
  (Hanger_Lane "Hanger Lane") (North_Acton "North Acton") (West_Acton "West Acton") (East_Acton "East Acton") (White_City "White City") (Shepherd_s_Bush "Shepherd's Bush")
  (Holland_Park "Holland Park") (Queensway "Queensway") (Lancaster_Gate "Lancaster Gate") (Marble_Arch "Marble Arch") (Bond_Street "Bond Street") (Tottenham_Court_Road "Tottenham Court Road")
  (Chancery_Lane "Chancery Lane") (St_Paul_s "St Paul's") (Bank "Bank") (Bethnal_Green "Bethnal Green") (Stratford "Stratford") (Leyton "Leyton") (Leytonstone "Leytonstone") (Wanstead "Wanstead")
  (Redbridge "Redbridge") (Gants_Hill "Gants Hill") (Newbury_Park "Newbury Park") (Barkingside "Barkingside") (Fairlop "Fairlop") (Hainault "Hainault") (Grange_Hill "Grange Hill")
  (Chigwell "Chigwell") (Roding_Valley "Roding Valley") (Woodford "Woodford") (Snaresbrook "Snaresbrook") (South_Woodford "South Woodford") (Buckhurst_Hill "Buckhurst Hill") (Loughton "Loughton")
  (Debden "Debden") (Theydon_Bois "Theydon Bois") (Epping "Epping")
  ;  Central Line Stations ^^^^              Jubilee Line Stations vvvvv
  (Stanmore "Stanmore") (Canons_Park "Canons Park") (Queensbury "Queensbury") (Kingsbury "Kingsbury") (Wembley_Park "Wembley Park") (Neasden "Neasden") (Dollis_Hill "Dollis Hill")
  (Willesden_Green "Willesden Green") (Kilburn "Kilburn") (West_Hampstead "West Hampstead") (Finchley_road "Finchley road") (Swiss_Cottage "Swiss Cottage") (St_John_s_Wood "St John's Wood")
  (Waterloo "Waterloo") (Southwark "Southwark") (London_Bridge "London Bridge") (Bermondsey "Bermondsey") (Canada_Water "Canada Water") (Canary_Wharf "Canary Wharf")
  (North_Greenwich "North Greenwich") (Canning_Town "Canning Town") 
  ;  Jubilee Line Stations ^^^^              Metropolitan Line Stations vvvvv
  (Finchley_Road "Finchley Road") (Preston_Road "Preston Road") (Northwick_Park "Northwick Park") (Harrow-on-the-Hill "Harrow-on-the-Hill") (West_Harrow "West Harrow")
  (North_Harrow "North Harrow") (Pinner "Pinner") (Northwood_Hills "Northwood Hills") (Northwood "Northwood") (Moor_Park "Moor Park") (Croxley "Croxley") (Watford "Watford")
  (Rickmansworth "Rickmansworth") (Chorleywood "Chorleywood") (Chalfont_&_Latimer "Chalfont & Latimer") (Chesham "Chesham") (Amersham "Amersham")
  ;  Metropolitan Line Stations ^^^^              Northern Line Stations vvvvv
  (Morden "Morden") (South_Wimbledon "South Wimbledon") (Colliers_Wood "Colliers Wood") (Tooting_Broadway "Tooting Broadway") (Tooting_Bec "Tooting Bec") (Balham "Balham")
  (Clapham_South "Clapham South") (Clapham_Common "Clapham Common") (Clapham_North "Clapham North") (Oval "Oval") (Kennington "Kennington") (Nine_Elms "Nine Elms")
  (Battersea_Power_Station "Battersea Power Station") (Charing_Cross "Charing Cross") (Goodge_Street "Goodge Street") (Mornington_Crescent "Mornington Crescent") (Camden_Town "Camden Town")
  (Elephant_&_Castle "Elephant & Castle") (Borough "Borough") (Old_Street "Old Street") (Angel "Angel") (Chalk_Farm "Chalk Farm") (Belsize_Park "Belsize Park") (Hampstead "Hampstead")
  (Golders_Green "Golders Green") (Brent_Cross "Brent Cross") (Hendon_Central "Hendon Central") (Colindale "Colindale") (Burnt_Oak "Burnt Oak") (Edgware "Edgware")
  (Kentish_Town "Kentish Town") (Tufnell_Park "Tufnell Park") (Archway "Archway") (Highgate "Highgate") (East_Finchley "East Finchley") (Finchley_Central "Finchley Central")
  (Mill_Hill_East "Mill Hill East") (West_Finchley "West Finchley") (Woodside_Park "Woodside Park") (Totteridge_&_Whetstone "Totteridge & Whetstone") (High_Barnet "High Barnet")
  ;  Northern Line Stations ^^^^              Bakerloo Line Stations vvvvv
  (Harrow_&_Wealdstone "Harrow & Wealdstone") (Kenton "Kenton") (South_Kenton "South Kenton") (North_Wembley "North Wembley") (Wembley_Central "Wembley Central")
  (Stonebridge_Park "Stonebridge Park") (Harlesden "Harlesden") (Willesden_Junction "Willesden Junction") (Kensal_Green "Kensal Green") (Queen_s_Park "Queen's Park")
  (Kilburn_Park "Kilburn Park") (Maida_Vale "Maida Vale") (Warwick_Avenue "Warwick Avenue") (Marylebone "Marylebone") (Regent_s_Park "Regent's Park") (Lambeth_North "Lambeth North")
  ;  Bakerloo Line Stations ^^^^
  ;  Waterloo & city line stations are defined above
  ;  Hammersmith& City line stations are defined above
  )

#;(define victoria_line_stations (list Walthamstow_Central Blackhorse_Road Tottenham_Hale Seven_Sisters Finsbury_Park Highbury_&_Islington Kings_Cross_St_Pancras Euston
                                     Warren_Street Oxford_Circus Green_Park Victoria Pimlico Vauxhall Stockwell Brixton))

(define london_underground_lines (list "victoria_line_stations" ;victoria_line_stations victoria_line_stations
                                       ;victoria_line_stations victoria_line_stations victoria_line_stations
                                       ;victoria_line_stations victoria_line_stations victoria_line_stations victoria_line_stations
                                       ))

                                       
;              vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv graphs vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
; A list (londonunderground) representing a Graph with nested lists showing edges for all the nodes(stations)
; this graph is built without showing all the edges for each line. Duplicates have been removed to prevent Errors in the DFS Algorithm

(define london_underground_graph (list
                                  (list Walthamstow_Central Blackhorse_Road) (list Blackhorse_Road Tottenham_Hale) (list Tottenham_Hale Seven_Sisters) (list Seven_Sisters Finsbury_Park)
                                  (list Finsbury_Park Highbury_&_Islington) (list Highbury_&_Islington Kings_Cross_St_Pancras) (list Kings_Cross_St_Pancras Euston) (list Euston Warren_Street)
                                  (list Warren_Street Oxford_Circus) (list Oxford_Circus Green_Park) (list Green_Park Victoria) (list Victoria Pimlico) (list Pimlico Vauxhall)
                                  (list Vauxhall Stockwell) (list Stockwell Brixton) (list Brixton Stockwell) (list Stockwell Vauxhall) (list Vauxhall Pimlico) (list Pimlico Victoria)
                                  (list Victoria Green_Park) (list Green_Park Oxford_Circus) (list Oxford_Circus Warren_Street) (list Warren_Street Euston) (list Euston Kings_Cross_St_Pancras)
                                  (list Kings_Cross_St_Pancras Highbury_&_Islington) (list Highbury_&_Islington Finsbury_Park) (list Finsbury_Park Seven_Sisters)
                                  (list Seven_Sisters Tottenham_Hale) (list Tottenham_Hale Blackhorse_Road) (list Blackhorse_Road Walthamstow_Central)
                                  ;  Victoria Line Edges without duplicates ^^^^              Circle Line Edges without duplicates vvvvv
                                  (list Hammersmith Goldhawk_Road) (list Goldhawk_Road Shepherd_s_Bush_Market) (list Shepherd_s_Bush_Market Wood_Lane) (list Wood_Lane Latimer_Road)
                                  (list Latimer_Road Ladbroke_Grove) (list Ladbroke_Grove Westbourne_Park) (list Westbourne_Park Royal_Oak) (list Royal_Oak Paddington)
                                  (list Paddington Edgware_Road) (list Edgware_Road Baker_Street) (list Baker_Street Great_Portland_Street) (list Great_Portland_Street Euston_Square)
                                  (list Euston_Square Kings_Cross_St_Pancras) (list Kings_Cross_St_Pancras Farringdon) (list Farringdon Barbican) (list Barbican Moorgate)
                                  (list Moorgate Liverpool_Street) (list Liverpool_Street Aldgate) (list Aldgate Tower_Hill) (list Tower_Hill Monument) (list Monument Cannon_Street)
                                  (list Cannon_Street Mansion_House) (list Mansion_House Blackfriars) (list Blackfriars Temple) (list Temple Embankment) (list Embankment Westminster)
                                  (list Westminster St_James_s_Park) (list St_James_s_Park Victoria) (list Victoria Sloane_Square) (list Sloane_Square South_Kensington)
                                  (list South_Kensington Gloucester_Road) (list Gloucester_Road High_Street_Kensington) (list High_Street_Kensington Notting_Hill_Gate)
                                  (list Notting_Hill_Gate Bayswater) (list Bayswater Paddington) (list Paddington Bayswater) (list Bayswater Notting_Hill_Gate)
                                  (list Notting_Hill_Gate High_Street_Kensington) (list High_Street_Kensington Gloucester_Road) (list Gloucester_Road South_Kensington)
                                  (list South_Kensington Sloane_Square) (list Sloane_Square Victoria) (list Victoria St_James_s_Park) (list St_James_s_Park Westminster)
                                  (list Westminster Embankment) (list Embankment Temple) (list Temple Blackfriars) (list Blackfriars Mansion_House) (list Mansion_House Cannon_Street)
                                  (list Cannon_Street Monument) (list Monument Tower_Hill) (list Tower_Hill Aldgate) (list Aldgate Liverpool_Street) (list Liverpool_Street Moorgate)
                                  (list Moorgate Barbican) (list Barbican Farringdon) (list Farringdon Kings_Cross_St_Pancras) (list Kings_Cross_St_Pancras Euston_Square)
                                  (list Euston_Square Great_Portland_Street) (list Great_Portland_Street Baker_Street) (list Baker_Street Edgware_Road) (list Edgware_Road Paddington)
                                  (list Paddington Royal_Oak) (list Royal_Oak Westbourne_Park) (list Westbourne_Park Ladbroke_Grove) (list Ladbroke_Grove Latimer_Road)
                                  (list Latimer_Road Wood_Lane) (list Wood_Lane Shepherd_s_Bush_Market) (list Shepherd_s_Bush_Market Goldhawk_Road) (list Goldhawk_Road Hammersmith)
                                  ;  Circle Line Edges without duplicates ^^^^              Piccadilly Line Edges without duplicates vvvvv
                                  (list Cockfosters Oakwood) (list Oakwood Southgate) (list Southgate Arnos_Grove) (list Arnos_Grove Bounds_Green) (list Bounds_Green Wood_Green)
                                  (list Wood_Green Turnpike_Lane) (list Turnpike_Lane Manor_House) (list Manor_House Finsbury_Park) (list Finsbury_Park Arsenal) (list Arsenal Holloway_Road)
                                  (list Holloway_Road Caledonian_Road) (list Caledonian_Road Kings_Cross_St_Pancras) (list Kings_Cross_St_Pancras Russell_Square) (list Russell_Square Holborn)
                                  (list Holborn Covent_Garden) (list Covent_Garden Leicester_Square) (list Leicester_Square Piccadilly_Circus) (list Piccadilly_Circus Green_Park)
                                  (list Green_Park Hyde_Park_Corner) (list Hyde_Park_Corner Knightsbridge) (list Knightsbridge South_Kensington)
                                  (list Gloucester_Road Earl_s_Court) (list Earl_s_Court Barons_Court) (list Barons_Court Hammersmith) (list Hammersmith Turnham_Green)
                                  (list Turnham_Green Acton_Town) (list Acton_Town Ealing_Common) (list Ealing_Common North_Ealing) (list North_Ealing Park_Royal) (list Park_Royal Alperton)
                                  (list Alperton Sudbury_Town) (list Sudbury_Town Sudbury_Hill) (list Sudbury_Hill South_Harrow) (list South_Harrow Rayners_Lane) (list Rayners_Lane Eastcote)
                                  (list Eastcote Ruislip_Manor) (list Ruislip_Manor Ruislip) (list Ruislip Ickenham) (list Ickenham Hillingdon) (list Hillingdon Uxbridge)
                                  (list Acton_Town South_Ealing) (list South_Ealing Northfields) (list Northfields Boston_Manor) (list Boston_Manor Osterley) (list Osterley Hounslow_East)
                                  (list Hounslow_East Hounslow_Central) (list Hounslow_Central Hounslow_West) (list Hounslow_West Hatton_Cross) (list Hatton_Cross Heathrow_Terminal_4)
                                  (list Heathrow_Terminal_4 Heathrow_Terminal_5) (list Heathrow_Terminal_5 Heathrow_Terminals_1_2_and_3) (list Heathrow_Terminals_1_2_and_3 Hatton_Cross)
                                  (list Hatton_Cross Hounslow_West) (list Hounslow_West Hounslow_Central) (list Hounslow_Central Hounslow_East) (list Hounslow_East Osterley)
                                  (list Osterley Boston_Manor) (list Boston_Manor Northfields) (list Northfields South_Ealing) (list South_Ealing Acton_Town) (list Uxbridge Hillingdon)
                                  (list Hillingdon Ickenham) (list Ickenham Ruislip) (list Ruislip Ruislip_Manor) (list Ruislip_Manor Eastcote) (list Eastcote Rayners_Lane)
                                  (list Rayners_Lane South_Harrow) (list South_Harrow Sudbury_Hill) (list Sudbury_Hill Sudbury_Town) (list Sudbury_Town Alperton) (list Alperton Park_Royal)
                                  (list Park_Royal North_Ealing) (list North_Ealing Ealing_Common) (list Ealing_Common Acton_Town) (list Acton_Town Turnham_Green) (list Turnham_Green Hammersmith)
                                  (list Hammersmith Barons_Court) (list Barons_Court Earl_s_Court) (list Earl_s_Court Gloucester_Road)
                                  (list South_Kensington Knightsbridge) (list Knightsbridge Hyde_Park_Corner) (list Hyde_Park_Corner Green_Park) (list Green_Park Piccadilly_Circus)
                                  (list Piccadilly_Circus Leicester_Square) (list Leicester_Square Covent_Garden) (list Covent_Garden Holborn) (list Holborn Russell_Square)
                                  (list Russell_Square Kings_Cross_St_Pancras) (list Kings_Cross_St_Pancras Caledonian_Road) (list Caledonian_Road Holloway_Road) (list Holloway_Road Arsenal)
                                  (list Arsenal Finsbury_Park) (list Finsbury_Park Manor_House) (list Manor_House Turnpike_Lane) (list Turnpike_Lane Wood_Green) (list Wood_Green Bounds_Green)
                                  (list Bounds_Green Arnos_Grove) (list Arnos_Grove Southgate) (list Southgate Oakwood) (list Oakwood Cockfosters)
                                  ;  Piccadilly Line Edges without duplicates ^^^^              District Line Edges without duplicates vvvvv
                                  (list Upminster Upminster_Bridge) (list Upminster_Bridge Hornchurch) (list Hornchurch Elm_Park) (list Elm_Park Dagenham_East)
                                  (list Dagenham_East Dagenham_Heathway) (list Dagenham_Heathway Becontree) (list Becontree Upney) (list Upney Barking) (list Barking East_Ham)
                                  (list East_Ham Upton_Park) (list Upton_Park Plaistow) (list Plaistow West_Ham) (list West_Ham Bromley-by-Bow) (list Bromley-by-Bow Bow_Road)
                                  (list Bow_Road Mile_End) (list Mile_End Stepney_Green) (list Stepney_Green Whitechapel) (list Whitechapel Aldgate_East) (list Aldgate_East Tower_Hill)
                                  (list Earl_s_Court High_Street_Kensington) (list High_Street_Kensington Earl_s_Court) (list Earl_s_Court Kensington_Olympia)
                                  (list Kensington_Olympia Earl_s_Court) (list Earl_s_Court West_Kensington) (list West_Kensington Barons_Court)
                                  (list Hammersmith Ravenscourt_Park) (list Ravenscourt_Park Stamford_Brook) (list Stamford_Brook Turnham_Green) (list Turnham_Green Chiswick_Park)
                                  (list Chiswick_Park Acton_Town) (list Ealing_Common Ealing_Broadway) (list Ealing_Broadway Ealing_Common)
                                  (list Acton_Town Chiswick_Park) (list Chiswick_Park Turnham_Green) (list Turnham_Green Gunnersbury) (list Gunnersbury Kew_Gardens) (list Kew_Gardens Gunnersbury)
                                  (list Gunnersbury Turnham_Green) (list Turnham_Green Stamford_Brook) (list Stamford_Brook Ravenscourt_Park) (list Ravenscourt_Park Hammersmith)
                                  (list Barons_Court West_Kensington) (list West_Kensington Earl_s_Court) (list Earl_s_Court West_Brompton)
                                  (list West_Brompton Fulham_Broadway) (list Fulham_Broadway Parsons_Green) (list Parsons_Green Putney_Bridge) (list Putney_Bridge East_Putney)
                                  (list East_Putney Southfields) (list Southfields Wimbledon_Park) (list Wimbledon_Park Wimbledon) (list Wimbledon Wimbledon_Park)
                                  (list Wimbledon_Park Southfields) (list Southfields East_Putney) (list East_Putney Putney_Bridge) (list Putney_Bridge Parsons_Green)
                                  (list Parsons_Green Fulham_Broadway) (list Fulham_Broadway West_Brompton) (list West_Brompton Earl_s_Court) 
                                  (list Tower_Hill Aldgate_East) (list Aldgate_East Whitechapel) (list Whitechapel Stepney_Green) (list Stepney_Green Mile_End)
                                  (list Mile_End Bow_Road) (list Bow_Road Bromley-by-Bow) (list Bromley-by-Bow West_Ham) (list West_Ham Plaistow) (list Plaistow Upton_Park)
                                  (list Upton_Park East_Ham) (list East_Ham Barking) (list Barking Upney) (list Upney Becontree) (list Becontree Dagenham_Heathway)
                                  (list Dagenham_Heathway Dagenham_East) (list Dagenham_East Elm_Park) (list Elm_Park Hornchurch) (list Hornchurch Upminster_Bridge)
                                  (list Upminster_Bridge Upminster)
                                  ;  District Line Edges without duplicates ^^^^              Central Line Edges without duplicates vvvvv
                                  (list West_Ruislip Ruislip_Gardens) (list Ruislip_Gardens South_Ruislip) (list South_Ruislip Northolt) (list Northolt Greenford) (list Greenford Perivale)
                                  (list Perivale Hanger_Lane) (list Hanger_Lane North_Acton) (list North_Acton West_Acton) (list West_Acton Ealing_Broadway) (list Ealing_Broadway West_Acton)
                                  (list West_Acton North_Acton) (list North_Acton East_Acton) (list East_Acton White_City) (list White_City Shepherd_s_Bush) (list Shepherd_s_Bush Holland_Park)
                                  (list Holland_Park Notting_Hill_Gate) (list Notting_Hill_Gate Queensway) (list Queensway Lancaster_Gate) (list Lancaster_Gate Marble_Arch)
                                  (list Marble_Arch Bond_Street) (list Bond_Street Oxford_Circus) (list Oxford_Circus Tottenham_Court_Road) (list Tottenham_Court_Road Holborn)
                                  (list Holborn Chancery_Lane) (list Chancery_Lane St_Paul_s) (list St_Paul_s Bank) (list Bank Liverpool_Street) (list Liverpool_Street Bethnal_Green)
                                  (list Bethnal_Green Mile_End) (list Mile_End Stratford) (list Stratford Leyton) (list Leyton Leytonstone) (list Leytonstone Wanstead) (list Wanstead Redbridge)
                                  (list Redbridge Gants_Hill) (list Gants_Hill Newbury_Park) (list Newbury_Park Barkingside) (list Barkingside Fairlop) (list Fairlop Hainault)
                                  (list Hainault Grange_Hill) (list Grange_Hill Chigwell) (list Chigwell Roding_Valley) (list Roding_Valley Woodford) (list Woodford Roding_Valley)
                                  (list Roding_Valley Chigwell) (list Chigwell Grange_Hill) (list Grange_Hill Hainault) (list Hainault Fairlop) (list Fairlop Barkingside)
                                  (list Barkingside Newbury_Park) (list Newbury_Park Gants_Hill) (list Gants_Hill Redbridge) (list Redbridge Wanstead) (list Wanstead Leytonstone)
                                  (list Leytonstone Snaresbrook) (list Snaresbrook South_Woodford) (list South_Woodford Woodford) (list Woodford Buckhurst_Hill) (list Buckhurst_Hill Loughton)
                                  (list Loughton Debden) (list Debden Theydon_Bois) (list Theydon_Bois Epping) (list Epping Theydon_Bois) (list Theydon_Bois Debden) (list Debden Loughton)
                                  (list Loughton Buckhurst_Hill) (list Buckhurst_Hill Woodford) (list Woodford South_Woodford) (list South_Woodford Snaresbrook) (list Snaresbrook Leytonstone)
                                  (list Leytonstone Leyton) (list Leyton Stratford) (list Stratford Mile_End) (list Mile_End Bethnal_Green) (list Bethnal_Green Liverpool_Street)
                                  (list Liverpool_Street Bank) (list Bank St_Paul_s) (list St_Paul_s Chancery_Lane) (list Chancery_Lane Holborn) (list Holborn Tottenham_Court_Road)
                                  (list Tottenham_Court_Road Oxford_Circus) (list Oxford_Circus Bond_Street) (list Bond_Street Marble_Arch) (list Marble_Arch Lancaster_Gate)
                                  (list Lancaster_Gate Queensway) (list Queensway Notting_Hill_Gate) (list Notting_Hill_Gate Holland_Park) (list Holland_Park Shepherd_s_Bush)
                                  (list Shepherd_s_Bush White_City) (list White_City East_Acton) (list East_Acton North_Acton) (list North_Acton Hanger_Lane) (list Hanger_Lane Perivale)
                                  (list Perivale Greenford) (list Greenford Northolt) (list Northolt South_Ruislip) (list South_Ruislip Ruislip_Gardens) (list Ruislip_Gardens West_Ruislip)
                                  ;  Central Line Edges without duplicates ^^^^              Hammersmith & City Line Edges without duplicates vvvvv
                                  (list Liverpool_Street Aldgate_East) (list Aldgate_East Liverpool_Street) 
                                  ;  Hammersmith & City Line Edges without duplicates ^^^^              Jubilee Line Edges without duplicates vvvvv
                                  (list Stanmore Canons_Park) (list Canons_Park Queensbury) (list Queensbury Kingsbury) (list Kingsbury Wembley_Park) (list Wembley_Park Neasden)
                                  (list Neasden Dollis_Hill) (list Dollis_Hill Willesden_Green) (list Willesden_Green Kilburn) (list Kilburn West_Hampstead) (list West_Hampstead Finchley_road)
                                  (list Finchley_road Swiss_Cottage) (list Swiss_Cottage St_John_s_Wood) (list St_John_s_Wood Baker_Street) (list Baker_Street Bond_Street)
                                  (list Bond_Street Green_Park) (list Green_Park Westminster) (list Westminster Waterloo) (list Waterloo Southwark) (list Southwark London_Bridge)
                                  (list London_Bridge Bermondsey) (list Bermondsey Canada_Water) (list Canada_Water Canary_Wharf) (list Canary_Wharf North_Greenwich)
                                  (list North_Greenwich Canning_Town) (list Canning_Town West_Ham) (list West_Ham Stratford) (list Stratford West_Ham) (list West_Ham Canning_Town)
                                  (list Canning_Town North_Greenwich) (list North_Greenwich Canary_Wharf) (list Canary_Wharf Canada_Water) (list Canada_Water Bermondsey)
                                  (list Bermondsey London_Bridge) (list London_Bridge Southwark) (list Southwark Waterloo) (list Waterloo Westminster) (list Westminster Green_Park)
                                  (list Green_Park Bond_Street) (list Bond_Street Baker_Street) (list Baker_Street St_John_s_Wood) (list St_John_s_Wood Swiss_Cottage)
                                  (list Swiss_Cottage Finchley_road) (list Finchley_road West_Hampstead) (list West_Hampstead Kilburn) (list Kilburn Willesden_Green)
                                  (list Willesden_Green Dollis_Hill) (list Dollis_Hill Neasden) (list Neasden Wembley_Park) (list Wembley_Park Kingsbury) (list Kingsbury Queensbury)
                                  (list Queensbury Canons_Park) (list Canons_Park Stanmore)
                                  ;  Jubilee Line Edges without duplicates ^^^^              Metropolitan Line Edges without duplicates vvvvv
                                  (list Baker_Street Finchley_Road) (list Finchley_Road Wembley_Park) (list Wembley_Park Preston_Road) (list Preston_Road Northwick_Park)
                                  (list Northwick_Park Harrow-on-the-Hill) (list Harrow-on-the-Hill West_Harrow) (list West_Harrow Rayners_Lane) 
                                  (list Rayners_Lane West_Harrow) (list West_Harrow Harrow-on-the-Hill)
                                  (list Harrow-on-the-Hill North_Harrow) (list North_Harrow Pinner) (list Pinner Northwood_Hills) (list Northwood_Hills Northwood) (list Northwood Moor_Park)
                                  (list Moor_Park Croxley) (list Croxley Watford) (list Watford Croxley) (list Croxley Moor_Park) (list Moor_Park Rickmansworth) (list Rickmansworth Chorleywood)
                                  (list Chorleywood Chalfont_&_Latimer) (list Chalfont_&_Latimer Chesham) (list Chesham Chalfont_&_Latimer) (list Chalfont_&_Latimer Amersham)
                                  (list Amersham Chalfont_&_Latimer) (list Chalfont_&_Latimer Chorleywood) (list Chorleywood Rickmansworth) (list Rickmansworth Moor_Park)
                                  (list Moor_Park Northwood) (list Northwood Northwood_Hills) (list Northwood_Hills Pinner) (list Pinner North_Harrow) (list North_Harrow Harrow-on-the-Hill)
                                  (list Harrow-on-the-Hill Northwick_Park) (list Northwick_Park Preston_Road) (list Preston_Road Wembley_Park) (list Wembley_Park Finchley_Road)
                                  (list Finchley_Road Baker_Street)
                                  ;  Metropolitan Line Edges without duplicates ^^^^              Northern Line Edges without duplicates vvvvv
                                  (list Morden South_Wimbledon) (list South_Wimbledon Colliers_Wood) (list Colliers_Wood Tooting_Broadway) (list Tooting_Broadway Tooting_Bec)
                                  (list Tooting_Bec Balham) (list Balham Clapham_South) (list Clapham_South Clapham_Common) (list Clapham_Common Clapham_North) (list Clapham_North Stockwell)
                                  (list Stockwell Oval) (list Oval Kennington) (list Kennington Nine_Elms) (list Nine_Elms Battersea_Power_Station) (list Battersea_Power_Station Nine_Elms)
                                  (list Nine_Elms Kennington) (list Kennington Waterloo) (list Waterloo Embankment) (list Embankment Charing_Cross) (list Charing_Cross Leicester_Square)
                                  (list Leicester_Square Tottenham_Court_Road) (list Tottenham_Court_Road Goodge_Street) (list Goodge_Street Warren_Street)
                                  (list Euston Mornington_Crescent) (list Mornington_Crescent Camden_Town) (list Camden_Town Mornington_Crescent) (list Mornington_Crescent Euston)
                                  (list Warren_Street Goodge_Street) (list Goodge_Street Tottenham_Court_Road) (list Tottenham_Court_Road Leicester_Square)
                                  (list Leicester_Square Charing_Cross) (list Charing_Cross Embankment) (list Embankment Waterloo) (list Waterloo Kennington) (list Kennington Elephant_&_Castle)
                                  (list Elephant_&_Castle Borough) (list Borough London_Bridge) (list London_Bridge Bank) (list Bank Moorgate) (list Moorgate Old_Street) (list Old_Street Angel)
                                  (list Angel Kings_Cross_St_Pancras) (list Euston Camden_Town) (list Camden_Town Chalk_Farm) (list Chalk_Farm Belsize_Park)
                                  (list Belsize_Park Hampstead) (list Hampstead Golders_Green) (list Golders_Green Brent_Cross) (list Brent_Cross Hendon_Central) (list Hendon_Central Colindale)
                                  (list Colindale Burnt_Oak) (list Burnt_Oak Edgware) (list Edgware Burnt_Oak) (list Burnt_Oak Colindale) (list Colindale Hendon_Central)
                                  (list Hendon_Central Brent_Cross) (list Brent_Cross Golders_Green) (list Golders_Green Hampstead) (list Hampstead Belsize_Park) (list Belsize_Park Chalk_Farm)
                                  (list Chalk_Farm Camden_Town) (list Camden_Town Kentish_Town) (list Kentish_Town Tufnell_Park) (list Tufnell_Park Archway) (list Archway Highgate)
                                  (list Highgate East_Finchley) (list East_Finchley Finchley_Central) (list Finchley_Central Mill_Hill_East) (list Mill_Hill_East Finchley_Central)
                                  (list Finchley_Central West_Finchley) (list West_Finchley Woodside_Park) (list Woodside_Park Totteridge_&_Whetstone) (list Totteridge_&_Whetstone High_Barnet)
                                  (list High_Barnet Totteridge_&_Whetstone) (list Totteridge_&_Whetstone Woodside_Park) (list Woodside_Park West_Finchley) (list West_Finchley Finchley_Central)
                                  (list Finchley_Central East_Finchley) (list East_Finchley Highgate) (list Highgate Archway) (list Archway Tufnell_Park) (list Tufnell_Park Kentish_Town)
                                  (list Kentish_Town Camden_Town) (list Camden_Town Euston) (list Kings_Cross_St_Pancras Angel) (list Angel Old_Street)
                                  (list Old_Street Moorgate) (list Moorgate Bank) (list Bank London_Bridge) (list London_Bridge Borough) (list Borough Elephant_&_Castle)
                                  (list Elephant_&_Castle Kennington) (list Kennington Oval) (list Oval Stockwell) (list Stockwell Clapham_North) (list Clapham_North Clapham_Common)
                                  (list Clapham_Common Clapham_South) (list Clapham_South Balham) (list Balham Tooting_Bec) (list Tooting_Bec Tooting_Broadway)
                                  (list Tooting_Broadway Colliers_Wood) (list Colliers_Wood South_Wimbledon) (list South_Wimbledon Morden)
                                  ;  Northern Line Edges without duplicates ^^^^              Bakerloo Line Edges without duplicates vvvvv
                                  (list Harrow_&_Wealdstone Kenton) (list Kenton South_Kenton) (list South_Kenton North_Wembley) (list North_Wembley  Wembley_Central)
                                  (list Wembley_Central Stonebridge_Park) (list Stonebridge_Park Harlesden) (list Harlesden Willesden_Junction) (list Willesden_Junction Kensal_Green)
                                  (list Kensal_Green Queen_s_Park) (list Queen_s_Park Kilburn_Park) (list Kilburn_Park Maida_Vale) (list Maida_Vale Warwick_Avenue)
                                  (list Warwick_Avenue Paddington) (list Edgware_Road Marylebone) (list Marylebone Baker_Street)
                                  (list Baker_Street Regent_s_Park) (list Regent_s_Park Oxford_Circus) (list Oxford_Circus Piccadilly_Circus) (list Piccadilly_Circus Charing_Cross)
                                  (list Waterloo Lambeth_North) (list Lambeth_North Elephant_&_Castle) (list Elephant_&_Castle Lambeth_North) (list Lambeth_North Waterloo) 
                                  (list Charing_Cross Piccadilly_Circus) (list Piccadilly_Circus Oxford_Circus) (list Oxford_Circus Regent_s_Park) (list Regent_s_Park Baker_Street)
                                  (list Baker_Street Marylebone) (list Marylebone Edgware_Road) (list Paddington Warwick_Avenue)
                                  (list Warwick_Avenue Maida_Vale) (list Maida_Vale Kilburn_Park) (list Kilburn_Park Queen_s_Park) (list Queen_s_Park Kensal_Green)
                                  (list Kensal_Green Willesden_Junction) (list Willesden_Junction Harlesden) (list Harlesden Stonebridge_Park) (list Stonebridge_Park Wembley_Central)
                                  (list Wembley_Central North_Wembley) (list North_Wembley South_Kenton) (list South_Kenton Kenton) (list Kenton Harrow_&_Wealdstone)
                                  ;  Bakerloo Line Edges without duplicates ^^^^              Waterloo & City Line Edges without duplicates vvvvv
                                  (list Bank Waterloo) (list Waterloo Bank)
                                  ;  Waterloo & City Line Edges without duplicates ^^^^
                                  ))

; vertices of the above graph, gethered through flattening and removing duplicates 
#; (remove-duplicates (flatten londonunderground))

(define combination_box_choices (remove-duplicates (flatten london_underground_graph)))

;         vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv Gui instantiation vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv


(define base_frame (new frame% [label "London Underground Route Planner"][width 400][height 900] ))
(define map_frame (new frame% [label "TFL Underground Map"][width 400][height 400] ))

(define global_message (new message% [parent base_frame][label "Please select your start and end Station\nThen click 'Confirm' to see your route"] ))                                                                                                                                        
(define input_panel (new vertical-panel% [parent base_frame][alignment '(center center)] ))
#;(define start_panel (new horizontal-panel% [parent input_panel][alignment '(center top)][min-width 300][min-height 20]))
#;(define end_panel (new horizontal-panel% [parent input_panel][alignment '(center top)][min-width 300][min-height 20]))

#;(define start_location_line (new combo-field% [parent start_panel][label " "][init-value "Choose Start Line"][choices london_underground_lines]))
(define start_location_station (new combo-field% [parent input_panel][label "Start Station"][init-value "Choose Start Station Here"][choices combination_box_choices]))
#;(define end_location_line (new combo-field% [parent end_panel][label " "][init-value "Choose Start Line"][choices london_underground_lines]))
(define end_location_station (new combo-field% [parent input_panel][label "End Station"][init-value "Choose End Station Here"][choices combination_box_choices]))

(define map_panel (new vertical-panel% [parent base_frame][min-width 300][min-height 300]))
(define london_map (new map-widget% [parent map_panel][position london][zoom 12]))

(define button_panel (new horizontal-panel% [parent input_panel][alignment '(center top)][min-width 300][min-height 30]))
(define line_map_panel (new horizontal-panel% [parent input_panel][alignment '(center top)][min-width 300][min-height 30]))
(define output_panel (new vertical-panel% [parent base_frame][alignment '(center top)][min-width 300][min-height 200]))

(define clear_button (new button% [parent button_panel][label "Clear"][callback (λ (button event)(send start_location_station set-value "")(send end_location_station set-value "")
                                                                           (send data_confirm set-label "Travel Planner Data:"))]))

(define switch (new button% [parent button_panel][label "Switch"][callback (λ (button event) (let ([x (list (send start_location_station get-value) (send end_location_station get-value))])
                                                                                               (send start_location_station set-value (second x))
                                                                                               (send end_location_station set-value(first x))
                                                                                               (send data_output set-value " ")))]))
(define confirm (new button% [parent button_panel][label "Confirm"]
                     [callback (λ (button event) (send data_confirm set-label "Route for your travel:")
                                 (send data_output set-value (string-join (dfs_shorter (send start_location_station get-value) (send end_location_station get-value)) "\n -> "))
                                 (send data_output set-label "\nScroll up to see Start location\n\n->Stops\n\n->End location")
                                 (println(string-join (dfs_shorter (send start_location_station get-value) (send end_location_station get-value)) " -> "))
                                 (println (dfs_shorter (send start_location_station get-value) (send end_location_station get-value))))]))

(define map_choice (new combo-field% [parent line_map_panel][init-value "Tube map.png"][label "Line Maps"][choices line_maps]
                                                                                                                                                     ))
(define tfl_map (new button% [parent line_map_panel] [label "Open Map"] [ callback (λ (button event)(send map_frame show #t))] ))

(define line_map_canvas (new canvas% [parent map_frame][stretchable-width 300][stretchable-height 200]
                             [style '(hscroll vscroll)]
                             [paint-callback (λ (canvas dc)(println (string-append "Images/" (send map_choice get-value)))
                                                (send dc draw-bitmap (read-bitmap (string-append "Images/" (send map_choice get-value))) 0 0)
                                                (send line_map_canvas get-virtual-size )(send line_map_canvas init-manual-scrollbars 10 10 1 1 1 1))]))

(define data_confirm (new message% [parent output_panel] [label "Travel Data:"] [min-width 200] [min-height 10]))
(define data_output (new text-field% [parent output_panel] [label "\nStart location\n\n->Stops\n\n->End location"] [min-width 250][min-height 270]))
(define global_message2 (new message% [parent base_frame] [label (string-append "Up to date as of "(date->string (seconds->date (current-seconds))))]))

(send base_frame show #t)
;          vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv code body vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(define (edge x graph) (map (λ (y) (first(rest y))) (filter(λ (y) (equal? (car y) x)) graph )))

(define (adults x graph) (map second (filter (λ (edge) (equal? x (first edge))) graph)))
; adults returns the nodes that x is related TO; > (adults "Euston" g) = '("Warren Street") Euston -> Warren St

(define (children x graph) (map first (filter (λ (edge) (equal? x (second edge))) graph)))
; children returns the nodes that x is related FROM; > (adults "Euston" g) = '("Kings Cross St Pancras") KC,SP -> Euston

(define (neighbours x graph) (append (children x graph) (adults x graph)))
; neighbours returns a list with the children and adult nodes of x; > (neighbours "Euston" g) = '("Kings Cross St Pancras" "Warren Street")

(define (degree x graph) (length (neighbours x graph)))
; degree counts the no. of neighbours a given node has within a graph

(define (nodes graph) (remove-duplicates (flatten graph)))
;function to get the vertices of an edge/graph ; > (nodes victorialine-graph)

; (define (connections x graph) (check for connections from a station within the graph)) <<<<<<<<<<<<<<<<< !!
; (define (access x graph)
; (define (toilets x graph)
;


(define (dfs_shorter x y) (depth_first_search x y (list )))

(define (depth_first_search x y workinglist)
  (cond
    [(equal? x y) (list y)]
    [(set-member? workinglist x) #f]
    [else
     (let ((route (dfs_helper (children x london_underground_graph)y (set-add workinglist x))))
       (if (equal? route #f) #f (cons x route)))] ))


(define (dfs_helper x y workinglist)
  (cond
    [(empty? x) #f]
    [else
     (let ((route (depth_first_search (first x) y workinglist)))
       (if (equal? route #f) (dfs_helper (rest x) y workinglist) route))] ))
