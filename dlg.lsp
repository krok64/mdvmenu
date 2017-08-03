(defun mdv:getDialogInput (/	    scale_hor	      scale_ver
			   el	    trassa   pk	      dist     land
			   pipe	    i	     deep     dlgShow  dlgLoaded
			   dcl_id   result
			  )
  (setq
    dlgLoaded T
    dlgShow T
    result nil
  )

  (if (= -1 (setq dcl_id (load_dialog "mdv.dcl")))
    (progn
      ;; There's a problem - display a message and set the
      ;; dialogLoaded flag to nil
      (princ "\nERROR: Cannot load mdv.dcl")
      (setq dlgLoaded nil)
    ) ;_ end of progn
  ) ;_ end of if

  (if (and dlgLoaded
	   (not (new_dialog "mdv_mainDialog" dcl_id))
      ) ;_ end of and
    (progn
      ;; There's a problem...
      (princ "\nERROR: Cannot show dialog mdv_mainDialog")
      (setq dlgShow nil)
    ) ;_ end of progn
  ) ;_ end of if

  (if (and dlgLoaded dlgShow)
    (progn
      (action_tile "cancel" "(done_dialog) (setq UserClick nil)")
      (action_tile
	"accept"
	(strcat
	  "(progn (setq scale_hor (atoi (get_tile \"mhor\")))"
	  "(setq scale_ver (atoi (get_tile \"mver\")))"
	  "(setq el (get_tile \"md_el\"))"
	  "(setq trassa (get_tile \"md_trassa\"))"
	  "(setq pk (get_tile \"md_pk\"))"
	  "(setq dist (get_tile \"md_dist\"))"
	  "(setq land (get_tile \"md_land\"))"
	  "(setq pipe (get_tile \"md_pipe\"))"
	  "(setq i (get_tile \"md_i\"))"
	  "(setq deep (get_tile \"md_deep\"))"
	  "(done_dialog) (setq UserClick T))"
	 ) ;_ end of strcat
      ) ;_ end of action_tile

      ;; Now that everything is set and ready to go, invoke the dialog.
      ;; Once it is on-screen, it controls the program flow, until the
      ;; user hits OK or cancel
      (start_dialog)

      ;; OK or cancel has been hit, you're out of the dialog.  Unload it
      (unload_dialog dcl_id)

      ;; Check for the value of the variable userClick.  This determines if
      ;; the user selected OK or cancel, and is represented by a value
      ;; of T or nil
      (if UserClick			; User clicked Ok
	;; Build the resulting data

	(progn
	  (setq	Result (list
			 (cons 1 scale_hor)
			 (cons 2 scale_ver)
			 (cons 3 el)
			 (cons 4 trassa)
			 (cons 5 pk)
			 (cons 6 dist)
			 (cons 7 land)
			 (cons 8 pipe)
			 (cons 9 i)
			 (cons 10 deep)
		       ) ;_ end of list
	  ) ;_ end of setq
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if
  result
)