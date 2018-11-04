open Graphql_ppx_base

let conv_pos pos = 
  {
    Lexing.pos_fname = pos.Source_pos.pos_fname;
    Lexing.pos_lnum = pos.Source_pos.pos_lnum;
    Lexing.pos_bol = pos.Source_pos.pos_bol;
    Lexing.pos_cnum = pos.Source_pos.pos_cnum;
  }

let conv_loc loc = 
  {
    Location.loc_start = conv_pos loc.Source_pos.loc_start;
    Location.loc_end = conv_pos loc.Source_pos.loc_end;
    Location.loc_ghost = loc.Source_pos.loc_ghost;
  }

let conv_pos_from_ast pos =
  {
    Source_pos.pos_fname = pos.Lexing.pos_fname;
    Source_pos.pos_lnum = pos.Lexing.pos_lnum;
    Source_pos.pos_bol = pos.Lexing.pos_bol;
    Source_pos.pos_cnum = pos.Lexing.pos_cnum;
  }

let conv_loc_from_ast loc = 
  {
    Source_pos.loc_start = conv_pos_from_ast loc.Location.loc_start;
    Source_pos.loc_end = conv_pos_from_ast loc.Location.loc_end;
    Source_pos.loc_ghost = loc.Location.loc_ghost;
  }

