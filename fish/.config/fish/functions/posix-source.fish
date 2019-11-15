function posix-source
	 echo "Setting the following env vars in shell and emacs:"
	 for i in (cat $argv)
	     set arr (echo $i |tr = \n)
	     switch $arr[1]
	     	    case "#*"
		    	 echo $arr > /dev/null
		    case ""
		         echo $arr > /dev/null
	            case "*"
		    	 echo $i
		         set -gx $arr[1] $arr[2]
			 emacsclient -e "(setenv \"$arr[1]\" \"$arr[2]\")" > /dev/null
             end
	 end
end