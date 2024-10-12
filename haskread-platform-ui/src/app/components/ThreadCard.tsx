import React from 'react';
import Link from 'next/link';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faComment, faShare, faThumbsDown } from '@fortawesome/free-solid-svg-icons'
import { faThumbsUp as f2 } from '@fortawesome/free-regular-svg-icons'

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
}) => {
    
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
                    <button className="flex items-center space-x-1 hover:text-blue-500">
                        <FontAwesomeIcon icon={ f2 } />
                        <span>{upvoteCount || 0}</span>
                    </button>
                    <button className="flex items-center space-x-1 hover:text-red-500">
                        <FontAwesomeIcon icon={faThumbsDown} />
                        <span>{downvoteCount || 0}</span>
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
