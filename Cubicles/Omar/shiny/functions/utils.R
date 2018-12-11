MyPalette <- c(
  Andy = '#e6194b', 
  Angela = '#fabebe', 
  Darryl = '#ffe119', 
  Dwight =  '#3cb44b', 
  Erin = '#f58231', 
  Jan =  '#911eb4',
  Jim = '#46f0f0', 
  Kelly = '#f032e6', 
  Kevin = '#bcf60c', 
  Michael = '#4363d8', 
  Oscar = '#008080', 
  Pam = '#e6beff',
  Phyllis ='#9a6324',
  Ryan = '#ffd8b1', 
  Toby = '#000000'
)

update_palette <- function(characters, MyPalette) {
  sec_characters <- characters[!characters %in% names(MyPalette)]
  MyPalette_sec <- rep('#686868', length(sec_characters))
  names(MyPalette_sec) <- sec_characters
  UpdatedPalette <- append(MyPalette, MyPalette_sec)
  return(UpdatedPalette)
}


