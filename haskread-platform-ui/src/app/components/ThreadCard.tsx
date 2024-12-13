import React from 'react';
import Link from 'next/link';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faComment, faShare, faThumbsDown as solidThumbsDown, faThumbsUp as solidThumbsUp } from '@fortawesome/free-solid-svg-icons'
import { faThumbsUp as regularThumbsup, faThumbsDown as regularThumbsDown } from '@fortawesome/free-regular-svg-icons'
import { useEffect, useState } from 'react';


interface ThreadCardProps {
    title: string;
    communityName: string;
    userName: string;
    createdAt: string;
    description?: string;
    upvoteCount?: number;
    downvoteCount?: number;
    commentCount?: number;
    threadIDForThreadInfo: number;
    userPostReaction: number;
}

const getUpIcon = (val : number) => {
    if (val == 1)
        return solidThumbsUp
    else
        return regularThumbsup
}

const getDownIcon = (val : number) => {
    if (val == 2)
        return solidThumbsDown 
    else
        return regularThumbsDown 
}

const ThreadCard: React.FC<ThreadCardProps> = ({
    title,
    communityName,
    userName,
    createdAt,
    description,
    upvoteCount,
    downvoteCount,
    commentCount,
    threadIDForThreadInfo,
    userPostReaction,
}) => {
    
    const [upvoteC, setUpvoteC] = useState(0);
    const [downvoteC, setDownvoteC] = useState(0);
    const [userUpvoteReaction, setUserUpvoteReaction] = useState(regularThumbsup);
    const [userDownvoteReaction, setUserDownvoteReaction] = useState(regularThumbsDown);

    useEffect(() => {
        const setData = async () => {
          if(upvoteCount)
            setUpvoteC(upvoteCount);
          if(downvoteCount)
            setDownvoteC(downvoteCount);
          setUserUpvoteReaction(getUpIcon(userPostReaction));
          setUserDownvoteReaction(getDownIcon(userPostReaction));
        };
        setData();
      },[])

    const updateUserReactions = async (isUpvotted : boolean) => {
        if(isUpvotted) {
            // user upvoted the comment
            // if user had already upvoted, then decrease the upvote count and change thumps up to regular
            // else check if user had downvoted the post, if yes then decrease the downvote count and change thumps down to regular
            // and increase the upvote count and change the thumps up to solid.
          if (userUpvoteReaction == solidThumbsUp){
            console.log("removing upvote");
            setUpvoteC(upvoteC-1);
            setUserUpvoteReaction(regularThumbsup);
          }else if(userDownvoteReaction == solidThumbsDown) {
           console.log("upvote is happening, removing downvote");
           setDownvoteC(downvoteC-1);
           setUserDownvoteReaction(regularThumbsDown);
           setUpvoteC(upvoteC+1);
           setUserUpvoteReaction(solidThumbsUp);
          }else{
            console.log("simple upvote is happening");
            setUpvoteC(upvoteC+1);
            setUserUpvoteReaction(solidThumbsUp);
          }
        }
        else {
            if(userDownvoteReaction == solidThumbsDown){
                console.log("removing downvote");
                setDownvoteC(downvoteC-1);
                setUserDownvoteReaction(regularThumbsDown);
            }else if(userUpvoteReaction == solidThumbsUp) {
                console.log("downvote is happening, removing upvotes");
                setUpvoteC(upvoteC - 1);
                setUserUpvoteReaction(regularThumbsup);
                setDownvoteC(downvoteC+1);
                setUserDownvoteReaction(solidThumbsDown);
            }else{
                console.log("simple downvote is happening");
                setDownvoteC(downvoteC+1);
                setUserDownvoteReaction(solidThumbsDown);
            }
        }
    }

    const handleUpvote = async () => {
        const token = localStorage.getItem('jwt_token');
        if (!token) {
          console.log('User is not authenticated');
          return;
        }
        try {
          const response = await fetch(`http://localhost:8085/api/v1/user/thread/upvote/${threadIDForThreadInfo}`, {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              Authorization: `Bearer ${token}`,
            },
          });
    
          if (response.ok) {
            updateUserReactions(true);
          } else {
            const res = await response.text();
            console.log(res);
          }
        } catch (error) {
          console.log('An error occurred: ' + error);
        }
      };

      const handleDownvote = async () => {
        const token = localStorage.getItem('jwt_token');
        if (!token) {
          console.log('User is not authenticated');
          return;
        }
        try {
          const response = await fetch(`http://localhost:8085/api/v1/user/thread/downvote/${threadIDForThreadInfo}`, {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              Authorization: `Bearer ${token}`,
            },
          });
    
          if (response.ok) {
            updateUserReactions(false);
          } else {
            const res = await response.text();
            console.log(res);
          }
        } catch (error) {
          console.log('An error occurred: ' + error);
        }
      };

    return (
        <div className="card-bg shadow-lg rounded-lg mb-6 overflow-hidden">
            <div className="flex justify-between items-center p-4 border-b">
                <h2 className="text-lg font-bold text-gray-800">
                    <Link href={`/view-thread/${threadIDForThreadInfo}`}>
                        {title}
                    </Link>
                </h2>
                <div className="text-sm text-right text-gray-500">
                    <p>
                        Community: <span className="font-semibold">{communityName}</span>
                    </p>
                    <p>
                        Created by <span className="font-semibold">{userName}</span>
                    </p>
                    <p>{new Date(createdAt).toLocaleDateString()}</p>
                </div>
            </div>
            {description && (
                <div className="p-4">
                    <p className="">
                        {description.substring(0, 100)}
                        {description.length > 100 && '...'}
                    </p>
                </div>
            )}
            <div className="flex justify-between items-center p-4 border-t">
                <div className="flex space-x-2 items-center">
                    <button className="flex items-center space-x-1 hover:text-blue-500" onClick={handleUpvote}>
                        <FontAwesomeIcon icon={ userUpvoteReaction } />
                        <span>{upvoteC}</span>
                    </button>
                    <button className="flex items-center space-x-1 hover:text-red-500" onClick={handleDownvote}>
                        <FontAwesomeIcon icon={ userDownvoteReaction } />
                        <span>{downvoteC}</span>
                    </button>
                    <span className="flex items-center space-x-1 ">
                        <FontAwesomeIcon icon={faComment} />
                        <span>{commentCount || 0}</span>
                    </span>
                </div>
                <button className="flex items-center space-x-1 text-gray-600 hover:text-green-500">
                    <FontAwesomeIcon icon={faShare} />
                    <span>Share</span>
                </button>
            </div>
        </div>
    );
};

export default ThreadCard;
