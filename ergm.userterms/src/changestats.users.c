/*  File src/changestats.users.c in package ergm.userterms, part of the Statnet suite
 *  of packages for network analysis, http://statnet.org .
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) at
 *  http://statnet.org/attribution
 *
 *  Copyright 2003-2013 Statnet Commons
 */

#include "changestats.users.h"




CHANGESTAT_FN(d_nsp2attr){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double p1attr, p2attr, tattr, hattr, uattr, vattr;
  

  p1attr = INPUT_PARAM[N_NODES];
  p2attr = INPUT_PARAM[N_NODES + 1];

  (CHANGE_STAT[0]) = 0;
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = IS_OUTEDGE(tail, head) ? -1 : 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr == p1attr && uattr == p2attr){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && IS_OUTEDGE(tail, v)) L2tu++;
        }
        (CHANGE_STAT[0]) += ((L2tu + echange == 1) - (L2tu == 1));
      }
    }
    
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){
      vattr = INPUT_PARAM[v-1];
      if (v != head && vattr == p1attr && hattr == p2attr){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && IS_INEDGE(head, u)) L2uh++;
        }
        (CHANGE_STAT[0]) += ((L2uh + echange == 1) - (L2uh == 1));
      }
    }
    

    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = IS_OUTEDGE(tail, head) ? -1 : 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr == p1attr && uattr == p2attr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v)) L2tu++;
        }
        (CHANGE_STAT[0]) -= ((L2tu + echange == 1) - (L2tu == 1));
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (tattr == p1attr && hattr == p2attr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (uattr == p1attr && hattr == p2attr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head)) L2uh++;
        }
        (CHANGE_STAT[0]) -= ((L2uh + echange == 1) - (L2uh == 1));
      }
    }
    
      (CHANGE_STAT[0]) -= echange*(L2th == 1);

    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);

}



CHANGESTAT_FN(d_nsp2attr3){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double exattr, tattr, hattr, uattr, vattr;
  
  /* *** don't forget tail -> head   
  CHANGE_STAT[0] = 0.0;*/  
  ZERO_ALL_CHANGESTATS(i);

  
  exattr = INPUT_PARAM[N_NODES];
  
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr != exattr && uattr != exattr && tattr != uattr){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && IS_OUTEDGE(tail, v)) L2tu++;
        }
        CHANGE_STAT[0] += ((L2tu + echange == 1) - (L2tu == 1));
      }
    }
    
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){
      vattr = INPUT_PARAM[v-1];
      if (v != head && vattr != exattr && hattr != exattr && vattr != hattr){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && IS_INEDGE(head, u)) L2uh++;
        }
        CHANGE_STAT[0] += ((L2uh + echange == 1) - (L2uh == 1));
      }
    }
    
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  

  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr != exattr && uattr != exattr && tattr != uattr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v)) L2tu++;
        }
        CHANGE_STAT[0] -= ((L2tu + echange == 1) - (L2tu == 1));
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (tattr != exattr && hattr != exattr && tattr != hattr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (uattr != exattr && hattr != exattr && uattr != hattr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head)) L2uh++;
        }
        CHANGE_STAT[0] -= ((L2uh + echange == 1) - (L2uh == 1));
      }
    }
      CHANGE_STAT[0] -= echange*(L2th == 1);

    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_nsp2attr2){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double p1attr, p2attr, battr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  
  p1attr = INPUT_PARAM[N_NODES + N_NODES];
  p2attr = INPUT_PARAM[N_NODES + N_NODES + 1];
  battr = INPUT_PARAM[N_NODES + N_NODES + 2];
  
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (u != tail && INPUT_PARAM[tail - 1] == p1attr && INPUT_PARAM[u-1] == p2attr && INPUT_PARAM[N_NODES + head - 1] == battr){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(tail, v)) L2tu++;
        }
        CHANGE_STAT[0] += ((L2tu + echange == 1) - (L2tu == 1));
      }
    }
    
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){
      if (v != head && INPUT_PARAM[v-1] == p1attr && INPUT_PARAM[head - 1] == p2attr && INPUT_PARAM[N_NODES + tail - 1] == battr){
        L2uh=ochange;
        /* step through outedges of v */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && INPUT_PARAM[N_NODES + u - 1] == battr && IS_INEDGE(head, u)) L2uh++;
        }
        CHANGE_STAT[0] += ((L2uh + echange == 1) - (L2uh == 1));
      }
    }
    
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
 
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (u != tail && INPUT_PARAM[tail-1] == p1attr && INPUT_PARAM[u-1] == p2attr && INPUT_PARAM[N_NODES + head - 1] == battr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(tail, v)) L2tu++;
        }
        CHANGE_STAT[0] -= ((L2tu + echange == 1) - (L2tu == 1));
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      if (INPUT_PARAM[tail-1] == p1attr && INPUT_PARAM[head-1] == p2attr && INPUT_PARAM[N_NODES + u - 1] == battr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (INPUT_PARAM[u-1] == p1attr && INPUT_PARAM[head-1] == p2attr && INPUT_PARAM[N_NODES + tail - 1] == battr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(v, head)) L2uh++;
        }
        CHANGE_STAT[0] -= ((L2uh + echange == 1) - (L2uh == 1));
      }
    }
    
      CHANGE_STAT[0] -= echange*(L2th == 1);

    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_dgwnspattr3){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange, exattr, tattr, hattr, uattr, vattr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  alpha = INPUT_PARAM[N_NODES+1];
  oneexpa = 1.0-exp(-alpha);
  
  exattr = INPUT_PARAM[N_NODES];
  
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    
    cumchange=0.0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr != exattr && uattr != exattr && tattr != uattr){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){
      vattr = INPUT_PARAM[v-1];
      if (v != head && vattr != exattr && hattr != exattr && vattr != hattr){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && IS_INEDGE(head, u)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh);
      }
    }
    
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  alpha = INPUT_PARAM[N_NODES+1];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr != exattr && uattr != exattr && tattr != uattr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (tattr != exattr && hattr != exattr && tattr != hattr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (uattr != exattr && hattr != exattr && uattr != hattr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) -= cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_dgwnspattr2){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange, p1attr, p2attr, battr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  alpha = INPUT_PARAM[N_NODES + N_NODES + 3];
  oneexpa = 1.0-exp(-alpha);
  
  p1attr = INPUT_PARAM[N_NODES + N_NODES];
  p2attr = INPUT_PARAM[N_NODES + N_NODES + 1];
  battr = INPUT_PARAM[N_NODES + N_NODES + 2];
  
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    
    cumchange=0.0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (u != tail && INPUT_PARAM[tail - 1] == p1attr && INPUT_PARAM[u-1] == p2attr && INPUT_PARAM[N_NODES + head - 1] == battr){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){
      if (v != head && INPUT_PARAM[v-1] == p1attr && INPUT_PARAM[head - 1] == p2attr && INPUT_PARAM[N_NODES + tail - 1] == battr){
        L2uh=ochange;
        /* step through outedges of v */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && INPUT_PARAM[N_NODES + u - 1] == battr && IS_INEDGE(head, u)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh);
      }
    }
    
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  alpha = INPUT_PARAM[N_NODES+N_NODES+3];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (u != tail && INPUT_PARAM[tail-1] == p1attr && INPUT_PARAM[u-1] == p2attr && INPUT_PARAM[N_NODES + head - 1] == battr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      if (INPUT_PARAM[tail-1] == p1attr && INPUT_PARAM[head-1] == p2attr && INPUT_PARAM[N_NODES + u - 1] == battr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (INPUT_PARAM[u-1] == p1attr && INPUT_PARAM[head-1] == p2attr && INPUT_PARAM[N_NODES + tail - 1] == battr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) -= cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_dgwnspattr){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange, p1attr, p2attr, tattr, hattr, uattr, vattr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  alpha = INPUT_PARAM[N_NODES+2];
  oneexpa = 1.0-exp(-alpha);
  
  p1attr = INPUT_PARAM[N_NODES];
  p2attr = INPUT_PARAM[N_NODES + 1];
  
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    
    cumchange=0.0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr == p1attr && uattr == p2attr){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){
      vattr = INPUT_PARAM[v-1];
      if (v != head && vattr == p1attr && hattr == p2attr){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && IS_INEDGE(head, u)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh);
      }
    }
    
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  alpha = INPUT_PARAM[N_NODES+2];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr == p1attr && uattr == p2attr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (tattr == p1attr && hattr == p2attr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (uattr == p1attr && hattr == p2attr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) -= cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_dgwespattr){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange, p1attr, p2attr, tattr, hattr, uattr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  alpha = INPUT_PARAM[N_NODES+2];
  oneexpa = 1.0-exp(-alpha);
  
  p1attr = INPUT_PARAM[N_NODES];
  p2attr = INPUT_PARAM[N_NODES + 1];
  

  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr == p1attr && uattr == p2attr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (tattr == p1attr && hattr == p2attr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (uattr == p1attr && hattr == p2attr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_dgwespattr2){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange, p1attr, p2attr, battr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  alpha = INPUT_PARAM[N_NODES + N_NODES + 3];
  oneexpa = 1.0-exp(-alpha);
  
  p1attr = INPUT_PARAM[N_NODES + N_NODES];
  p2attr = INPUT_PARAM[N_NODES + N_NODES + 1];
  battr = INPUT_PARAM[N_NODES + N_NODES + 2];
    
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (u != tail && INPUT_PARAM[tail-1] == p1attr && INPUT_PARAM[u-1] == p2attr && INPUT_PARAM[N_NODES + head - 1] == battr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      if (INPUT_PARAM[tail-1] == p1attr && INPUT_PARAM[head-1] == p2attr && INPUT_PARAM[N_NODES + u - 1] == battr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (INPUT_PARAM[u-1] == p1attr && INPUT_PARAM[head-1] == p2attr && INPUT_PARAM[N_NODES + tail - 1] == battr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(INPUT_PARAM[N_NODES + v - 1] == battr && IS_OUTEDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_dgwespattr3){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange, exattr, tattr, hattr, uattr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  alpha = INPUT_PARAM[N_NODES+1];
  oneexpa = 1.0-exp(-alpha);
  
  exattr = INPUT_PARAM[N_NODES];
   
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr != exattr && uattr != exattr && tattr != uattr && IS_OUTEDGE(tail, u)){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (tattr != exattr && hattr != exattr && tattr != hattr && IS_OUTEDGE(tail, u)){
        L2th++;
      }
      if (uattr != exattr && hattr != exattr && uattr != hattr && IS_OUTEDGE(u, tail)){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
  
}


D_CHANGESTAT_FN(d_edgecovgwnspattr) { 
  Edge e, f;
  int i, echange, ochange;
  int L2th, L2tu, L2uh;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange;
  
  int nrow, brcat, beginmat;
  
  CHANGE_STAT[0] = 0.0;
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  brcat = (long int)(INPUT_PARAM[1]);
  nrow = (long int)(INPUT_PARAM[2]);
  beginmat = (long int)(nrow+3);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    
    cumchange=0.0;
    tail=TAIL(i); head=HEAD(i);
    ochange = -IS_OUTEDGE(tail,head);
    echange = 2*ochange + 1;
    
    
    /* step through outedges of head */
    for(e = MIN_OUTEDGE(head); (u=OUTVAL(e))!=0; e=NEXT_OUTEDGE(e)) { 
      if (u != tail && INPUT_PARAM[beginmat+((tail-1)*nrow+(u-1))] == 1 && INPUT_PARAM[head+2] == brcat){
        L2tu=ochange; /* L2tu will be # shrd prtnrs of (tail,u) not incl. head */
    /* step through inedges of u, incl. (head,u) itself */
    for(f = MIN_INEDGE(u); (v=INVAL(f))!=0; f=NEXT_INEDGE(f)) {
      if(IS_OUTEDGE(tail,v)  && INPUT_PARAM[v+2] == brcat) L2tu++;
    }
    cumchange += pow(oneexpa,(double)L2tu); /* sign corrected below */
      }
    }
    /* step through inedges of tail */
    for(e = MIN_INEDGE(tail); (u=INVAL(e))!=0; e=NEXT_INEDGE(e)) {
      if (u != head && INPUT_PARAM[beginmat+((u-1)*nrow+(head-1))] == 1 && INPUT_PARAM[tail+2] == brcat){
        L2uh=ochange; /* L2uh will be # shrd prtnrs of (u,head) not incl. tail */
    /* step through outedges of u , incl. (u,tail) itself */
    for(f = MIN_OUTEDGE(u);(v=OUTVAL(f))!=0; f=NEXT_OUTEDGE(f)){
      if(IS_OUTEDGE(v,head) && INPUT_PARAM[v+2] == brcat) L2uh++;
    }
    cumchange += pow(oneexpa,(double)L2uh); /* sign corrected below */
      }
    }
    
    CHANGE_STAT[0] += echange*cumchange;
    
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){      
    
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
    
    
    /* step through outedges of head  */
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (IS_OUTEDGE(tail, u) && INPUT_PARAM[beginmat+((tail-1)*nrow+(u-1))] == 1 && INPUT_PARAM[head+2] == brcat){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v) && INPUT_PARAM[v+2] == brcat) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    /* step through inedges of head */
    
    STEP_THROUGH_INEDGES(head, e, u){
      if (IS_OUTEDGE(tail, u) && INPUT_PARAM[beginmat+((tail-1)*nrow+(head-1))] == 1 && INPUT_PARAM[u+2] == brcat){
        L2th++;
      }
      if (IS_OUTEDGE(u, tail) && INPUT_PARAM[beginmat+((u-1)*nrow+(head-1))] == 1 && INPUT_PARAM[tail+2] == brcat){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head) && INPUT_PARAM[v+2] == brcat) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    
    (CHANGE_STAT[0]) -= cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}


D_CHANGESTAT_FN(d_edgecovgwnsp) { 
  Edge e, f;
  int i, echange, ochange;
  int L2th, L2tu, L2uh;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange;
  
  int nrow;
  
  
  
  CHANGE_STAT[0] = 0.0;
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  nrow = (long int)(INPUT_PARAM[1]);
  
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    
    cumchange=0.0;
    tail=TAIL(i); head=HEAD(i);
    ochange = -IS_OUTEDGE(tail,head);
    echange = 2*ochange + 1;
    
    
    /* step through outedges of head */
    for(e = MIN_OUTEDGE(head); (u=OUTVAL(e))!=0; e=NEXT_OUTEDGE(e)) { 
      if (u != tail && INPUT_PARAM[2+((tail-1)*nrow+(u-1))] == 1){
        L2tu=ochange; /* L2tu will be # shrd prtnrs of (tail,u) not incl. head */
    /* step through inedges of u, incl. (head,u) itself */
    for(f = MIN_INEDGE(u); (v=INVAL(f))!=0; f=NEXT_INEDGE(f)) {
      if(IS_OUTEDGE(tail,v)) L2tu++;
    }
    cumchange += pow(oneexpa,(double)L2tu); /* sign corrected below */
      }
    }
    /* step through inedges of tail */
    for(e = MIN_INEDGE(tail); (u=INVAL(e))!=0; e=NEXT_INEDGE(e)) {
      if (u != head && INPUT_PARAM[2+((u-1)*nrow+(head-1))] == 1){
        L2uh=ochange; /* L2uh will be # shrd prtnrs of (u,head) not incl. tail */
    /* step through outedges of u , incl. (u,tail) itself */
    for(f = MIN_OUTEDGE(u);(v=OUTVAL(f))!=0; f=NEXT_OUTEDGE(f)){
      if(IS_OUTEDGE(v,head)) L2uh++;
    }
    cumchange += pow(oneexpa,(double)L2uh); /* sign corrected below */
      }
    }
    
    CHANGE_STAT[0] += echange*cumchange;
    
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){      
    
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
    
    
    /* step through outedges of head  */
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (IS_OUTEDGE(tail, u) && INPUT_PARAM[2+((tail-1)*nrow+(u-1))] == 1){
        L2tu=ochange;
        /* step through inedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    /* step through inedges of head */
    
    STEP_THROUGH_INEDGES(head, e, u){
      if (IS_OUTEDGE(tail, u) && INPUT_PARAM[2+((tail-1)*nrow+(head-1))] == 1){
        L2th++;
      }
      if (IS_OUTEDGE(u, tail) && INPUT_PARAM[2+((u-1)*nrow+(head-1))] == 1){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_OUTEDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    
    (CHANGE_STAT[0]) -= cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
    
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_path2attr2){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh, L2th;
  Vertex tail, head, u, v, nnodes;
  double oneexpa, cumchange, tattr, hattr, uattr, vattr, battr;
  
  nnodes = N_NODES;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  oneexpa = 1.0;
  
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    
    cumchange=0.0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){ /*t=a1, h=b, u=a3*/
    tattr = INPUT_PARAM[tail - 1];
      hattr = INPUT_PARAM[nnodes + head - 1];
      uattr = INPUT_PARAM[nnodes + nnodes + u - 1];
      battr = INPUT_PARAM[nnodes + nnodes + nnodes + head - 1];
      if (u != tail && 
          INPUT_PARAM[4*nnodes] == tattr &&
          INPUT_PARAM[4*nnodes+1] == hattr &&
          INPUT_PARAM[4*nnodes+2] == uattr &&
          INPUT_PARAM[4*nnodes+3] == battr
      ){
        L2tu=ochange;
        /* step through outedges of u ...... note sure if this is necessary or if I need to cut it or edit it*/
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){ /*v=a1, t=b, h=a3*/
    vattr = INPUT_PARAM[v - 1];
      tattr = INPUT_PARAM[nnodes + tail - 1];
      hattr = INPUT_PARAM[nnodes + nnodes + head - 1];
      battr = INPUT_PARAM[nnodes + nnodes + nnodes + tail - 1];
      if (v != head && 
          INPUT_PARAM[4*nnodes] == vattr &&
          INPUT_PARAM[4*nnodes+1] == tattr &&
          INPUT_PARAM[4*nnodes+2] == hattr &&
          INPUT_PARAM[4*nnodes+3] == battr
      ){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && IS_INEDGE(head, u)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh);
      }
    }
    
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  oneexpa = 1.0;
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /*step through outedges of head again*/
    STEP_THROUGH_OUTEDGES(head, e, u){ /*t=a1, h=b, u=a3*/
    tattr = INPUT_PARAM[tail - 1];
      hattr = INPUT_PARAM[nnodes + head - 1];
      uattr = INPUT_PARAM[nnodes + nnodes + u - 1];
      battr = INPUT_PARAM[nnodes + nnodes + nnodes + head - 1];
      if (u != tail && 
          INPUT_PARAM[4*nnodes] == tattr &&
          INPUT_PARAM[4*nnodes+1] == hattr &&
          INPUT_PARAM[4*nnodes+2] == uattr &&
          INPUT_PARAM[4*nnodes+3] == battr && 
          IS_UNDIRECTED_EDGE(tail, u)
      ){
        L2tu=ochange;
        /* step through inedges of u 
        STEP_THROUGH_INEDGES(u, f, v){
        if(IS_OUTEDGE(tail, v)) L2tu++;
        } */
        cumchange += pow(oneexpa,(double)L2tu);
      }
  }
    
    /* step through inedges of head */
    STEP_THROUGH_INEDGES(head, e, u){ /*t=a1, u=b, h=a3 ???????*/
    tattr = INPUT_PARAM[tail - 1];
      uattr = INPUT_PARAM[nnodes + u - 1];
      hattr = INPUT_PARAM[nnodes + nnodes + head - 1];
      battr = INPUT_PARAM[nnodes + nnodes + nnodes + u - 1];
      if (INPUT_PARAM[4*nnodes] == tattr &&
          INPUT_PARAM[4*nnodes+1] == uattr &&
          INPUT_PARAM[4*nnodes+2] == hattr &&
          INPUT_PARAM[4*nnodes+3] == battr && 
          IS_OUTEDGE(tail, u)
      ){
        L2th++;
      }
      if (INPUT_PARAM[4*nnodes] == uattr && /**/
    INPUT_PARAM[4*nnodes+1] == tattr &&
      INPUT_PARAM[4*nnodes+2] == hattr &&
      INPUT_PARAM[4*nnodes+3] == battr &&
      IS_OUTEDGE(u, tail)
      ){
        L2uh=ochange;
          /* step through outedges of u 
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_OUTEDGE(v, head)) L2uh++;
            }*/
          cumchange += pow(oneexpa,(double)L2uh) ;
      }
}
  
  if(0 < 100.0){
    cumchange += (double)L2th;
  }else{
    cumchange += (double)L2th;
  }
  cumchange  = echange*cumchange;
  (CHANGE_STAT[0]) -= cumchange;
  TOGGLE_IF_MORE_TO_COME(i);
}
UNDO_PREVIOUS_TOGGLES(i);

}


D_CHANGESTAT_FN(d_path2attr) { 
  double change;
  int i;
  Edge e;
  Vertex tail, head, node3, nnodes;
  double tailattr, headattr, n3attr, battr;
  
  nnodes = N_NODES;
  
  /* *** don't forget tail -> head */    
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    change = IS_OUTEDGE(tail = TAIL(i), head = HEAD(i))? -1.0 : 1.0 ;
    
    STEP_THROUGH_INEDGES(tail, e, node3) { /* step through inedges of tail */
  n3attr = INPUT_PARAM[node3 - 1]; /* a1 attr list */
  tailattr = INPUT_PARAM[nnodes + tail - 1]; /* a2 attr list; tail is broker in this scenario*/
  headattr = INPUT_PARAM[nnodes + nnodes + head - 1]; /* a3 attr list*/
  battr = INPUT_PARAM[nnodes + nnodes + nnodes + tail - 1]; /* ; tail is broker in this scenario*/
  if (node3 != head && 
      INPUT_PARAM[nnodes + nnodes + nnodes + nnodes] == n3attr && 
      INPUT_PARAM[nnodes + nnodes + nnodes + nnodes + 1] == tailattr &&
      INPUT_PARAM[nnodes + nnodes + nnodes + nnodes + 2] == headattr &&
      INPUT_PARAM[nnodes + nnodes + nnodes + nnodes + 3] == battr 
        /*&&  IS_OUTEDGE(node3, head) == 0 */
  ) {
    CHANGE_STAT[0] += change;
  }
    }
    
    STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through inedges of tail */
        tailattr = INPUT_PARAM[tail - 1]; /* a1 attr list */
        headattr = INPUT_PARAM[nnodes + head - 1]; /* a2 attr list; head is broker in this scenario*/
        n3attr = INPUT_PARAM[nnodes + nnodes + node3 - 1]; /* a3 attr list*/
        battr = INPUT_PARAM[nnodes + nnodes + nnodes + head - 1]; /* ; head is broker in this scenario*/
        if (node3 != tail && 
            INPUT_PARAM[nnodes + nnodes + nnodes + nnodes] == tailattr && 
            INPUT_PARAM[nnodes + nnodes + nnodes + nnodes + 1] == headattr &&
            INPUT_PARAM[nnodes + nnodes + nnodes + nnodes + 2] == n3attr &&
            INPUT_PARAM[nnodes + nnodes + nnodes + nnodes + 3] == battr 
              /*&&  IS_OUTEDGE(tail, node3) == 0*/
        ) {
          CHANGE_STAT[0] += change;
        }
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}


CHANGESTAT_FN(d_gwb1nspnew) { 
  Edge e, f;
  int i, echange, ochange;
  int L2th, L2tu, L2uh;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange;
  
  CHANGE_STAT[0] = 0.0;
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){      
    cumchange=0.0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
    STEP_THROUGH_INEDGES(head, e, u){
      if (u != tail){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
        }
        /* step through inedges of u 
        STEP_THROUGH_INEDGES(u, f, v){
        if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
        } */
        cumchange += pow(oneexpa,(double)L2tu);
      }
  }
    
    STEP_THROUGH_INEDGES(tail, e, u){
      if (u != head){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
        }
        /* step through inedges of u 
        STEP_THROUGH_INEDGES(u, f, v){
        if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
        }*/
        cumchange += pow(oneexpa,(double)L2uh);
      }
}
    
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
}
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
    
    STEP_THROUGH_INEDGES(head, e, u){
      if (IS_UNDIRECTED_EDGE(u, tail)){
        L2th++;
        L2tu=ochange;
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(u, f, v){
          if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
          if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu) +
          pow(oneexpa,(double)L2uh) ;
      }
    }
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) -= cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_gwb2nspnew) { 
  Edge e, f;
  int i, echange, ochange;
  int L2th, L2tu, L2uh;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange;
  
  CHANGE_STAT[0] = 0.0;
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){      
    cumchange=0.0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (u != tail){
        L2tu=ochange;
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    STEP_THROUGH_OUTEDGES(tail, e, u){
      if (u != head){
        L2uh=ochange;
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh);
      }
    }
    
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
  alpha = INPUT_PARAM[0];
  oneexpa = 1.0-exp(-alpha);
  
  /* *** don't forget tail -> head */    
  FOR_EACH_TOGGLE(i){
    cumchange=0.0;
    L2th=0;
    ochange = IS_OUTEDGE(tail=TAIL(i), head=HEAD(i)) ? -1 : 0;
    echange = 2*ochange + 1;
    /* step through outedges of head  */
    STEP_THROUGH_OUTEDGES(head, e, u){
      if (IS_UNDIRECTED_EDGE(u, tail)){
        L2th++;
        L2tu=ochange;
        L2uh=ochange;
        
        STEP_THROUGH_INEDGES(u, f, v){
          if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
          if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu) +
          pow(oneexpa,(double)L2uh) ;
      }
    }
    
    
    if(alpha < 100.0){
      cumchange += exp(alpha)*(1.0-pow(oneexpa,(double)L2th)) ;
    }else{
      cumchange += (double)L2th;
    }
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) -= cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
  }
  
  UNDO_PREVIOUS_TOGGLES(i);
  
}


CHANGESTAT_FN(d_dgwdspattr){
  Edge e, f;
  int i, echange, ochange;
  int L2tu, L2uh;
  Vertex tail, head, u, v;
  double alpha, oneexpa, cumchange, p1attr, p2attr, tattr, hattr, uattr, vattr;
  
  /* *** don't forget tail -> head */    
  CHANGE_STAT[0] = 0.0;
  alpha = INPUT_PARAM[N_NODES+2];
  oneexpa = 1.0-exp(-alpha);
  
  p1attr = INPUT_PARAM[N_NODES];
  p2attr = INPUT_PARAM[N_NODES + 1];
  
  FOR_EACH_TOGGLE(i){
    tail = TAIL(i); 
    head = HEAD(i);
    tattr = INPUT_PARAM[tail-1];
    hattr = INPUT_PARAM[head-1];
    
    cumchange=0.0;
    ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
    echange = 2*ochange + 1;
    
    /* step through outedges of head */
    STEP_THROUGH_OUTEDGES(head, e, u){
      uattr = INPUT_PARAM[u-1];
      if (u != tail && tattr == p1attr && uattr == p2attr){
        L2tu=ochange;
        /* step through outedges of u */
        STEP_THROUGH_INEDGES(u, f, v){
          if(v != head && IS_OUTEDGE(tail, v)) L2tu++;
        }
        cumchange += pow(oneexpa,(double)L2tu);
      }
    }
    /*    STEP_THROUGH_INEDGES(head, e, u){
    if (u != tail && hattr == p2attr){
    L2tu=ochange;
    STEP_THROUGH_INEDGES(u, f, v){
    if(v != head && INPUT_PARAM[v-1] == p1attr && IS_INEDGE(tail, v)) L2tu++;
    }
    cumchange += pow(oneexpa,(double)L2tu);
    }
  }
    
    STEP_THROUGH_OUTEDGES(tail, e, v){
    if (v != head && tattr == p1attr){
    L2uh=ochange;
    STEP_THROUGH_OUTEDGES(v, f, u){
    if(u != tail && INPUT_PARAM[u-1] == p2attr && IS_OUTEDGE(head, u)) L2uh++;
    }
    cumchange += pow(oneexpa,(double)L2uh);
    }
    }
    */
    /* step through inedges of tail */
    STEP_THROUGH_INEDGES(tail, e, v){
      vattr = INPUT_PARAM[v-1];
      if (v != head && vattr == p1attr && hattr == p2attr){
        L2uh=ochange;
        /* step through outedges of u */
        STEP_THROUGH_OUTEDGES(v, f, u){
          if(u != tail && IS_INEDGE(head, u)) L2uh++;
        }
        cumchange += pow(oneexpa,(double)L2uh);
      }
    }
    
    cumchange  = echange*cumchange;
    (CHANGE_STAT[0]) += cumchange;
    TOGGLE_IF_MORE_TO_COME(i);
    }
  
  UNDO_PREVIOUS_TOGGLES(i);
  }
  
  
  CHANGESTAT_FN(d_gwdspattr){
    Edge e, f;
    int i, echange, ochange;
    int L2tu, L2uh;
    Vertex tail, head, u, v;
    double alpha, oneexpa, cumchange, p1attr, p2attr, tattr, hattr, uattr;
    
    /* *** don't forget tail -> head */    
    CHANGE_STAT[0] = 0.0;
    alpha = INPUT_PARAM[N_NODES+2];
    oneexpa = 1.0-exp(-alpha);
    
    p1attr = INPUT_PARAM[N_NODES];
    p2attr = INPUT_PARAM[N_NODES + 1];
    
    FOR_EACH_TOGGLE(i){
      tail = TAIL(i); 
      head = HEAD(i);
      tattr = INPUT_PARAM[tail-1];
      hattr = INPUT_PARAM[head-1];
      
      cumchange=0.0;
      ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
      echange = 2*ochange + 1;
      
      /* step through outedges of head */
      STEP_THROUGH_OUTEDGES(head, e, u){
        if (u != tail){
          uattr = INPUT_PARAM[u-1];
          L2tu=ochange;
          /* step through outedges of u */
          if(MIN(tattr,uattr) == MIN(p1attr,p2attr) && MAX(tattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            cumchange += pow(oneexpa,(double)L2tu);
          }
        }
      }
      /* step through inedges of head */
      STEP_THROUGH_INEDGES(head, e, u){
        if (u != tail){
          uattr = INPUT_PARAM[u-1];
          L2tu=ochange;
          /* step through outedges of u */
          if(MIN(tattr,uattr) == MIN(p1attr,p2attr) && MAX(tattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            cumchange += pow(oneexpa,(double)L2tu);
          }
        }
      }
      
      /* step through outedges of tail  */
      STEP_THROUGH_OUTEDGES(tail, e, u){
        if (u != head){
          uattr = INPUT_PARAM[u-1];
          L2uh=ochange;
          /* step through outedges of u */
          if(MIN(hattr,uattr) == MIN(p1attr,p2attr) && MAX(hattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            cumchange += pow(oneexpa,(double)L2uh);
          }
        }
      }
      /* step through inedges of tail */
      STEP_THROUGH_INEDGES(tail, e, u){
        if (u != head){
          uattr = INPUT_PARAM[u-1];
          L2uh=ochange;
          /* step through outedges of u */
          if(MIN(hattr,uattr) == MIN(p1attr,p2attr) && MAX(hattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            cumchange += pow(oneexpa,(double)L2uh);
          }
        }
      }
      
      
      cumchange  = echange*cumchange;
      (CHANGE_STAT[0]) += cumchange;
      TOGGLE_IF_MORE_TO_COME(i);
    }
    
    UNDO_PREVIOUS_TOGGLES(i);
  }
  
  
  CHANGESTAT_FN(d_cycle2pendants){
    Edge e, f;
    int i, echange, ochange;
    int L2tu, L2uh;
    Vertex tail, head, u, v;
    double alpha, oneexpa, cumchange, p1attr, p2attr, tattr, hattr, uattr;
    
    /* *** don't forget tail -> head */    
    CHANGE_STAT[0] = 0.0;
    alpha = INPUT_PARAM[N_NODES+2];
    oneexpa = 1.0-exp(-alpha);
    
    p1attr = INPUT_PARAM[N_NODES];
    p2attr = INPUT_PARAM[N_NODES + 1];
    
    FOR_EACH_TOGGLE(i){
      tail = TAIL(i); 
      head = HEAD(i);
      tattr = INPUT_PARAM[tail-1];
      hattr = INPUT_PARAM[head-1];
      
      cumchange=0.0;
      ochange = IS_OUTEDGE(tail, head) ? -1 : 0;
      echange = 2*ochange + 1;
      
      /* step through outedges of head */
      STEP_THROUGH_OUTEDGES(head, e, u){
        if (u != tail){
          uattr = INPUT_PARAM[u-1];
          L2tu=ochange;
          /* step through outedges of u */
          if(MIN(tattr,uattr) == MIN(p1attr,p2attr) && MAX(tattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            cumchange += pow(oneexpa,(double)L2tu);
          }
        }
      }
      /* step through inedges of head */
      STEP_THROUGH_INEDGES(head, e, u){
        if (u != tail){
          uattr = INPUT_PARAM[u-1];
          L2tu=ochange;
          /* step through outedges of u */
          if(MIN(tattr,uattr) == MIN(p1attr,p2attr) && MAX(tattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, tail)) L2tu++;
            }
            cumchange += pow(oneexpa,(double)L2tu);
          }
        }
      }
      
      /* step through outedges of tail  */
      STEP_THROUGH_OUTEDGES(tail, e, u){
        if (u != head){
          uattr = INPUT_PARAM[u-1];
          L2uh=ochange;
          /* step through outedges of u */
          if(MIN(hattr,uattr) == MIN(p1attr,p2attr) && MAX(hattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            cumchange += pow(oneexpa,(double)L2uh);
          }
        }
      }
      /* step through inedges of tail */
      STEP_THROUGH_INEDGES(tail, e, u){
        if (u != head){
          uattr = INPUT_PARAM[u-1];
          L2uh=ochange;
          /* step through outedges of u */
          if(MIN(hattr,uattr) == MIN(p1attr,p2attr) && MAX(hattr,uattr) == MAX(p1attr,p2attr)){
            STEP_THROUGH_OUTEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            /* step through inedges of u */
            STEP_THROUGH_INEDGES(u, f, v){
              if(IS_UNDIRECTED_EDGE(v, head)) L2uh++;
            }
            cumchange += pow(oneexpa,(double)L2uh);
          }
        }
      }
      
      
      cumchange  = echange*cumchange;
      (CHANGE_STAT[0]) += cumchange;
      TOGGLE_IF_MORE_TO_COME(i);
    }
    
    UNDO_PREVIOUS_TOGGLES(i);
  }
  
  CHANGESTAT_FN(d_mindegree) {
    Vertex t, h, node3;
    int i, mindeg, hdeg, tdeg;
    Edge e;
    int attrflag;
    double t_nodecov, h_nodecov;
    
    ZERO_ALL_CHANGESTATS(i);
    FOR_EACH_TOGGLE(i) {
      t = TAIL(i); h = HEAD(i);
      attrflag = INPUT_PARAM[0];
      mindeg = INPUT_PARAM[1];
      if(attrflag==0){
        tdeg = IN_DEG[t]+OUT_DEG[t];
        hdeg = IN_DEG[h]+OUT_DEG[h];
        CHANGE_STAT[0] += IS_UNDIRECTED_EDGE(t,h) ?
        - (tdeg==mindeg) - (hdeg==mindeg) :
        (tdeg==mindeg-1) + (hdeg==mindeg-1);
      }else{
        t_nodecov = INPUT_PARAM[t+1];
        h_nodecov = INPUT_PARAM[h+1];
        if (t_nodecov == h_nodecov) {
          tdeg = 0;
          STEP_THROUGH_OUTEDGES(t, e, node3) { /* step through outedges of tail */
            if(INPUT_PARAM[node3+1]==t_nodecov){++tdeg;}
          }
          STEP_THROUGH_INEDGES(t, e, node3) { /* step through inedges of tail */
            if(INPUT_PARAM[node3+1]==t_nodecov){++tdeg;}
          }
          hdeg = 0;
          STEP_THROUGH_OUTEDGES(h, e, node3) { /* step through outedges of head */
            if(INPUT_PARAM[node3+1]==h_nodecov){++hdeg;}
          }
          STEP_THROUGH_INEDGES(h, e, node3) { /* step through inedges of head */
            if(INPUT_PARAM[node3+1]==h_nodecov){++hdeg;}
          }
          CHANGE_STAT[0] += IS_UNDIRECTED_EDGE(t,h) ?
          - (tdeg==mindeg) - (hdeg==mindeg) :
          (tdeg==mindeg-1) + (hdeg==mindeg-1);
        }else{
          CHANGE_STAT[0] = 0;
        }
      }
      TOGGLE_IF_MORE_TO_COME(i);
    }
    UNDO_PREVIOUS_TOGGLES(i);
  }
  
  